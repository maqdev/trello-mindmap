import com.typesafe.config.ConfigFactory
import java.io.{FileInputStream, File}
import play.api.http.Status
import play.api.libs.json.JsObject
import play.api.libs.json.{JsValue, JsObject}
import play.api.libs.ws.WS
import scala.xml._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.xml.transform.{RuleTransformer, RewriteRule}

//import java.util.concurrent.TimeUnit._


object TrelloMindmapSync{

  lazy val config = ConfigFactory.load
  lazy val trelloAppKey = config.getString("trello.appKey")
  lazy val trelloUserToken = config.getString("trello.userToken")
  lazy val targetFileName = "tasks.opml"
  lazy val tagetFileNameEncoding = "UTF-8"

  lazy val boardUrls = config.getString("trello.boards").split(',')

  object logger {
    def info(message: String) = println(message)
    def debug(message: String) = println(message)
    def error(message: String) = println(message)
  }

  def fetch(resource: String, params: Option[String] = None) : Future[JsValue] = {
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

  def emptyXml =
    <opml version="1.0">
      <head></head>
      <body>
        <outline id="1" text="Trello Tasks">
          <outline id="2" text="[Inbox]">
          </outline>
        </outline>
      </body>
    </opml>

  object TaskState extends Enumeration { val New, Existing, Removed = Value }
  case class Task(id:String, idBoard:String, name: String, shortUrl: String, state: TaskState.Value)

  def main(args: Array[String]) {

    val targetFile = new File(targetFileName);
    val original =
      if (targetFile.exists)
        XML.load(new FileInputStream(targetFile))
      else
        emptyXml

    val tasks = scala.collection.mutable.Map[String,Task]()

    val fetchBoards = boardUrls.map { boardUrl =>
      fetch("board/"+boardUrl+"/cards") map {
        case response =>
          response.as[List[JsObject]] map { case j: JsObject =>
            Task(
              (j \ "id").as[String],
              (j \ "idBoard").as[String],
              (j \ "name").as[String],
              (j \ "shortUrl").as[String],
              TaskState.New
            )
          }
      }
    }

    fetchBoards.map { boardFuture =>
      boardFuture map { boardTasks =>

        tasks ++= boardTasks map { t => (t.shortUrl, t)}

        for (
          outline <- (original \\ "outline");
          link = ((outline \ "@link").text);
          id = ((outline \ "@id").text);
          text = ((outline \ "@text").text);

          // task
          if (link.startsWith("URL: https://trello.com/c/"));
          task = tasks.get(link)
        ) {
          if (task.isDefined)
            tasks(link) = task.get.copy(state = TaskState.Existing)
          else
            tasks(link) = Task(id, "", text, link, TaskState.Removed)
        }
      }
    }

    fetchBoards map { boardFuture => Await.result(boardFuture, 60 seconds) } // todo: find better solution

    object AddNewTasks extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case e: Elem =>
          if (e.label == "outline" && (e \ "@text").text == "[Inbox]" && (e \ "@url").text.isEmpty) {

            val newTasks = tasks.values.filter(t => t.state == TaskState.New)

            val newTasksOutline = newTasks.map { task =>
              <outline id={task.id} text={task.name} type="link" url={"URL: "+task.shortUrl}>
              </outline>
            }
            (e.copy(child = e.child ++ newTasksOutline) % Attribute(null, "alt", "An image of a hamster", Null))

          } else
          if (e.label == "outline" && (e \ "@url").text.startsWith("URL: https://trello.com/c/")) {

            val shortUrl = (e \ "@url").text.substring("URL: ".length)
            val t = tasks.get(shortUrl).get
            val updatedElement =
              t.state match {
                case TaskState.Existing => e % Attribute(null, "alt", "An image of a hamster", Null)
                case TaskState.Removed => e % Attribute(null, "alt", "An image of a hamster", Null)
              }
            updatedElement
          }
        else
          e
      case _ => n
    }
  }

  println("Total tasks: " + tasks.size)

  val rule = new RuleTransformer(AddNewTasks)
  val newXml = rule.apply(original)
  println(newXml)


    //Thread.sleep(10000)
    // fetchBoards map { boardFuture => Await.result(boardFuture, 60 seconds) }

    //println("Halting...")
    //Runtime.getRuntime.halt(-1)
    //System.exit(0)
	}
}