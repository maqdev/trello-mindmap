import com.typesafe.config.ConfigFactory
import java.io._
import java.util.zip._
import org.apache.commons.io.IOUtils
import play.api.http.Status
import play.api.libs.json.JsObject
import play.api.libs.json.{JsValue, JsObject}
import play.api.libs.ws.WS
import scala.Some
import scala.xml._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.xml.transform.{RuleTransformer, RewriteRule}
import resource._

//import java.util.concurrent.TimeUnit._


object TrelloMindmapSync {

  lazy val config = ConfigFactory.load
  lazy val trelloAppKey = config.getString("trello.appKey")
  lazy val trelloUserToken = config.getString("trello.userToken")
  lazy val inputFileName = config.getString("input.fileName")
  lazy val outputFileName = config.getString("output.fileName")
  lazy val boardUrls = config.getString("trello.boards").split(',')

  object logger {
    def info(message: String) = println(message)

    def debug(message: String) = println(message)

    def error(message: String) = println(message)
  }

  def fetch(resource: String, params: Option[String] = None): Future[JsValue] = {
    var query = "https://api.trello.com/1/" + resource + "?key=" + trelloAppKey + "&token=" + trelloUserToken + params.getOrElse("")
    logger.debug("REQ: " + query)
    val f = WS.url(query).get
    f.map {
      case response =>
        if (response.status != Status.OK) {
          throw new Exception("Failed with code: " + response.status + " " + response.body)
        }
        else
          response.json
    }
  }

  object TaskState extends Enumeration {
    val New, Existing, Removed = Value
  }

  case class Task(id: String, idBoard: String, name: String, shortUrl: String, state: TaskState.Value)

  def main(args: Array[String]) {

    val tasks = scala.collection.mutable.Map[String, Task]()

    val fetchBoards = boardUrls.map {
      boardUrl =>
        fetch("board/" + boardUrl + "/cards") map {
          case response =>
            response.as[List[JsObject]] map {
              case j: JsObject =>
                Task(
                  (j \ "id").as[String],
                  (j \ "idBoard").as[String],
                  (j \ "name").as[String],
                  (j \ "shortUrl").as[String],
                  TaskState.New
                )
            }
        }
    } toSeq

    fetchBoards.map {
      boardFuture =>
        boardFuture map {
          boardTasks =>

            tasks ++= boardTasks map {
              t => (t.shortUrl, t)
            }
        }
    }

    val f = Future.sequence(fetchBoards)
    val count = Await.result(f, 60 seconds).foldLeft(0)(_ + _.size)
    logger.info("Count of tasks: " + count)
    tasks.map(t => println(t))

    val inputFile = new File(inputFileName)
    for (
      inputFileZip <- managed(new ZipFile(inputFile));
      outputFileZip <- managed(new ZipOutputStream(new FileOutputStream(outputFileName)))) {

      import collection.JavaConverters._
      val entries = inputFileZip.entries.asScala
      entries foreach { e =>
        println(e.getName)
        outputFileZip.putNextEntry(new ZipEntry(e.getName))
        val in = inputFileZip.getInputStream(e)
        if (e.getName()=="content.xml")
          convertContentXml(in, outputFileZip, tasks)
        else
          IOUtils.copy(in, outputFileZip)
        outputFileZip.closeEntry()
      }
    }

    //Thread.sleep(10000)
    // fetchBoards map { boardFuture => Await.result(boardFuture, 60 seconds) }

    println("Halting...")
    Runtime.getRuntime.halt(-1)
    //System.exit(0) - doesn't work
  }

  def convertContentXml(in: InputStream, out: OutputStream, tasks: scala.collection.mutable.Map[String, Task]) = {

    val original = XML.load(in)

    for (
      topic <- (original \\ "topic");
      url = cleanUrl(((topic \ "@href").text));
      id = ((topic \ "@id").text);
      text = ((topic \ "title").text);

      // task
      if (url.startsWith("https://trello.com/c/"));
      task = tasks.get(url)
    ) {

      if (task.isDefined)
        tasks(url) = task.get.copy(state = TaskState.Existing)
      else
        tasks(url) = Task(id, "", text, url, TaskState.Removed)
    }

    val newTasks = tasks.values.filter(t => t.state == TaskState.New)

    object AddNewTasks extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case e: Elem =>
          if (e.label == "outline") {
            val text = (e \ "@text").text
            val url = cleanUrl((e \ "@url").text)
            if (text == "[Inbox]" && url.isEmpty) {
              val newTasksOutline = newTasks.map {
                task =>
                  <outline id={task.id} text={task.name} type="link" url={"URL: " + task.shortUrl}>
                  </outline>
              }
              e.copy(child = e.child ++ newTasksOutline)
            } else
            if (url.startsWith("https://trello.com/c/")) {

              tasks.get(url) match {
                case Some(Task(_, _, name, shortUrl, TaskState.Removed)) =>
                  e % Attribute(null, "text", prependDeleted(name), Null)

                case Some(Task(id, _, name, _, TaskState.Existing)) =>
                  e % Attribute(null, "text", name, Attribute(null, "id", id, Null))

                case _ => e
              }
            }
            else
              e
          }
          else e
        case _ => n
      }
    }

    val rule = new RuleTransformer(AddNewTasks)
    val newXml = rule.apply(original)

    logger.info("New task count: " + newTasks.size)
    for (task <- newTasks) {
      logger.info("New task: " + task.shortUrl + " " + task.name)
    }

    for (task <- tasks.values.filter(t => t.state == TaskState.Removed)) {
      logger.info("Removed task: " + task.shortUrl + " " + task.name)
    }

    for (writer <- managed(new OutputStreamWriter(out))) {
      XML.write(writer, newXml, "UTF-8", false, null)
    }

    //XML.save(out, newXml)
    newXml
  }

  def prependDeleted(s: String) = if (s.startsWith("[DELETED!]: ")) s else "[DELETED!]: " + s

  def cleanUrl(s: String) = if (s.startsWith("URL:")) s.substring(4).trim() else s
}