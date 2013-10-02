import java.io.{FileInputStream, File}
import java.util.concurrent.Executors._
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
import play.api.http.Status
import play.api.libs.json.{JsValue, JsObject, JsArray}
import play.api.libs.ws.{Response, WS}
import scala.util.parsing.json.JSON
import scala.util.{Failure, Success}
import scala.xml._
import scala.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._
import scala.xml.transform.{RuleTransformer, RewriteRule}

//import java.util.concurrent.TimeUnit._

import com.typesafe.scalalogging.slf4j.Logging

object TrelloMindmapSync{

  val trelloAppKey = ""
  val trelloUserToken = ""
  val targetFileName = "tasks.opml"
  val tagetFileNameEncoding = "UTF-8"

  val boardUrl = "fODfytyp"

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
          <outline id="3" text="AION">
            <outline id="4f3517aa0bc744f35614a44c" text="[4g-client] необходимо создать ограничитель скорости закачки" link="https://trello.com/c/s7m58KA8"></outline>
            <outline id="4f3517aa0bc744f35614a44c" text="[4g-client] необходимо создать ограничитель скорости закачки" link="https://trello.com/c/s7m58KA9"></outline>
          </outline>
        </outline>
      </body>
    </opml>

  case class Task(id:String, idBoard:String, name: String, shortUrl: String)

	def main(args: Array[String]) {

    val targetFile = new File(targetFileName);
    val original =
      if (targetFile.exists)
        XML.load(new FileInputStream(targetFile))
      else
        emptyXml

    val futureBoardTasks = fetch("board/"+boardUrl+"/cards") map {
      case response =>
        response.as[List[JsObject]] map { case j: JsObject =>
          val shortUrl = (j \ "shortUrl").as[String]
          val task =
            Task(
              (j \ "id").as[String],
              (j \ "idBoard").as[String],
              (j \ "name").as[String],
              shortUrl
            )
          (shortUrl, task)
        } toMap
    }

    futureBoardTasks.map { tasks =>

      val newTasks = scala.collection.mutable.Map[String,Task](tasks.toSeq: _*)
      val existingTasks = scala.collection.mutable.Map[String,Task]()
      val deletedTasks = scala.collection.mutable.Set[String]()

      for (
        outline <- (original \\ "outline");
        link = ((outline \ "@link").text);

        // task
        if (link.startsWith("https://trello.com/c/"));
        task = tasks.get(link)
      ) {
        if (task.isDefined)
          existingTasks += link -> task.get
        else
          deletedTasks += link
        newTasks.remove(link)
      }

      val newTasksMap = newTasks.toMap
      println(newTasksMap.size)

      object AddNewTasks extends RewriteRule {
        override def transform(n: Node): Seq[Node] = n match {
          case e: Elem =>
            if (e.label == "outline" && (e \\ "@text").text.contains("[Inbox]") && (e \\ "@link").text.isEmpty) {
              val newTasksOutline = newTasks.values.map { task =>
                <outline id={task.id} text={task.name} link={task.shortUrl}></outline>
              }
              e.copy(child = e.child ++ newTasksOutline)
            }
            else
              e
          case _ => n
        }
      }

      val rule = new RuleTransformer(AddNewTasks)
      val newXml = rule.apply(original)
      println(newXml)
    }

    //Thread.sleep(10000)
    Await.result(futureBoardTasks, 60 seconds)

    //println("Halting...")
    //Runtime.getRuntime.halt(-1)
    //System.exit(0)
	}
}