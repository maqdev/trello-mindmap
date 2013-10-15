
import com.typesafe.config.ConfigFactory
import java.io._
import java.util.zip._
import org.apache.commons.io.IOUtils
import org.joda.time.DateTime
import play.api.http.Status
import play.api.libs.json.{JsArray, JsValue, JsObject}
import play.api.libs.ws.WS
import scala.Some

import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future

import com.codecommit.antixml._

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

  case class Task(id: String, idBoard: String, idList: String, name: String, shortUrl: String, date: DateTime, state: TaskState.Value)

  def processMindMap(tasks: Map[String, Task], boards: Map[String, String], lists: Map[String, String]) = {
    val inputFile = new File(inputFileName)
    for (
      inputFileZip <- managed(new ZipFile(inputFile));
      outputFileZip <- managed(new ZipOutputStream(new FileOutputStream(outputFileName + ".tmp")))) {

      import collection.JavaConverters._
      val entries = inputFileZip.entries.asScala
      entries foreach {
        e =>
          outputFileZip.putNextEntry(new ZipEntry(e.getName))
          val in = inputFileZip.getInputStream(e)
          if (e.getName() == "CDDocument.xml")
            convertContentXml(in, outputFileZip, tasks, boards, lists)
          else
            IOUtils.copy(in, outputFileZip)
          outputFileZip.closeEntry()
      }
    }

    // overwrite
    val outputFile = new File(outputFileName)
    if (outputFile.exists()) {
      outputFile.delete
    }

    val tempOutputFile = new File(outputFileName + ".tmp")
    tempOutputFile.renameTo(outputFile)
  }

  def main(args: Array[String]) {

    val fetchBoards = boardUrls.map {
      boardUrl =>
        fetch("board/" + boardUrl + "/") map {
          case j: JsObject =>
            ((j \ "id").as[String], (j \ "name").as[String])
        }
    }

    val fetchLists = boardUrls.map {
      boardUrl =>
        fetch("board/" + boardUrl + "/lists/") map {
          case response: JsArray => response.as[List[JsObject]] map {
            case j: JsObject =>
              ((j \ "id").as[String], (j \ "name").as[String])
          }
        }
    }

    val fetchBoardCards = boardUrls.map {
      boardUrl =>
        fetch("board/" + boardUrl + "/cards") map {
          case response: JsArray =>
            response.as[List[JsObject]] map {
              case j: JsObject =>
                Task(
                  (j \ "id").as[String],
                  (j \ "idBoard").as[String],
                  (j \ "idList").as[String],
                  (j \ "name").as[String],
                  (j \ "shortUrl").as[String],
                  DateTime.parse((j \ "dateLastActivity").as[String]),
                  TaskState.New
                )
            }
        }
    }

    val fboards = Future.sequence(fetchBoards.toSeq)
    val flists = Future.sequence(fetchLists.toSeq)
    val fcards = Future.sequence(fetchBoardCards.toSeq)

    fcards.onSuccess {
      case boardCards =>
        val tasks = boardCards.flatten.map(t => (t.shortUrl, t)).toMap

        fboards.onSuccess {
          case boardResults =>
            val boards = boardResults.toMap

            flists.onSuccess {
              case listResults =>

                val lists = listResults.flatten.toMap
                try {
                  processMindMap(tasks, boards, lists)
                }
                catch {
                  case x => println(x)
                }
                println("Shutting down")
                System.exit(0)
            }
        }
    }

    flists.onFailure {
      case x =>
        println(x)
        Runtime.getRuntime.halt(-1)
    }

    fcards.onFailure {
      case x =>
        println(x)
        Runtime.getRuntime.halt(-1)
    }

    fboards.onFailure {
      case x =>
        println(x)
        Runtime.getRuntime.halt(-1)
    }

    //val count = Await.result(fcards, 60 seconds).foldLeft(0)(_ + _.size)
    //Await.result


    //println("Shutting down")
    //Runtime.getRuntime.halt(-1)
    //System.exit(0) - doesn't work
  }

  def topicToTask(xmlTask: Elem, tasks: Map[String, Task]) = {

    def textOf(g: Group[Node]) : String =
      g.toList.map{
        case Elem(_, _, _, _, children) => textOf(children)
        case t: Text => t.text
        case c: CDATA => c.text
        case _ => ""
      }.mkString

    val links = (xmlTask \ "Hyperlinks" \ "Hyperlink").flatMap(_.attr(QName("xlink","href")))

    if (!links.isEmpty && links.head.startsWith("https://trello.com/c/")) {
      val url = links.head;
      val id = textOf((xmlTask \ "ID"))
      val text = textOf((xmlTask \ "Name"))
      val date = DateTime.now

      val task = tasks.get(url)
      if (task.isDefined)
        Some(task.get.copy(state = TaskState.Existing))
      else
        Some(Task(id, "", "", text, url, date, TaskState.Removed))
    }
    else
      None
  }

  def convertContentXml(in: InputStream, out: OutputStream, boardTasks: Map[String, Task], boards: Map[String, String], lists: Map[String, String]) = {

    println("Board tasks: " + boardTasks.size)

    val original = XML.fromInputStream(in)

    val mindmapTasks = (original \\ "Task").map(t => topicToTask(t, boardTasks)).flatten
    println("Mindmap tasks: " + mindmapTasks.size)

    val tasks = boardTasks ++ mindmapTasks.map {
      m => (m.shortUrl, m)
    }

    val newTasks = tasks.values.filter(t => t.state == TaskState.New)
    println("Total tasks: " + tasks.size)
    println("New tasks: " + newTasks.size)

    val groupedNewTasks = {
      newTasks groupBy (_.idList) groupBy (_._2.map(_.idBoard).head)
    } // map { a => (a._1 -> a._2) }

    val newXml = original

    for (task <- newTasks) {
      logger.info("New task: " + boards(task.idBoard) + " / " + lists(task.idList) + " / " + task.shortUrl + " " + task.name)
    }

    for (task <- tasks.values.filter(t => t.state == TaskState.Removed)) {
      logger.info("Removed task: " + task.shortUrl + " " + task.name)
    }

    val writer = new OutputStreamWriter(out) {
      override def close = {
        // close call should be ignored
      }
    }

    original.writeTo(writer)
    //XML.write(writer, newXml, "UTF-8", false, null)
    writer.flush()
  }
}