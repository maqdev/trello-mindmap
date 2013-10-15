
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

  def textOf(g: Group[Node]) : String =
    g.toList.map{
      case Elem(_, _, _, _, children) => textOf(children)
      case t: Text => t.text
      case c: CDATA => c.text
      case _ => ""
    }.mkString

  def topicToTask(xmlTask: Elem, tasks: Map[String, Task]) = {

    val links = (xmlTask \ "Hyperlinks" \ "Hyperlink").flatMap(_.attr(QName("xlink","href")))
    val text = textOf((xmlTask \ "Name"))
    val urlLinks = !links.isEmpty && links.head.startsWith("https://trello.com/c/")
    if (urlLinks || text.contains("https://trello.com/c/")) {
      val url = if (urlLinks) links.head else {
        val start = text.indexOf("https://trello.com/c/")
        val end = text.indexWhere(c => c.isSpaceChar || List(']','[',',',')','(','.').contains(c))
        if (end > 0)
          text.substring(start, end)
        else
          text.substring(start)
      }

      val id = textOf((xmlTask \ "ID"))
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

    val projects = original \ "Projects"

    val mindmapTasks = (projects \ "Project" \ "Task").map(t => topicToTask(t, boardTasks)).flatten

    var maxTaskId = (projects \ "Project" \ "Task").map(t => textOf(t \ "ID").toLong).max

    val project = (projects \ "Project").head
    var maxProjectOutlineId = 1 + (project \ "Task").map(t => textOf(t \ "OutlineNumber").split('.').head.toLong).max

    println("Mindmap tasks: " + mindmapTasks.size)

    val tasks = boardTasks ++ mindmapTasks.map {
      m => (m.shortUrl, m)
    }

    val newTasks = tasks.values.filter(t => t.state == TaskState.New)
    println("Total tasks: " + tasks.size)
    println("New tasks: " + newTasks.size)

    def genTaskName(t: Task) = t.name + " / " + lists(t.idList) + " " + t.shortUrl

    def newTaskXml(outline: String, name: String, url: Option[String] = None) : Elem ={
      maxTaskId += 1
      Elem("Task", Attributes(),Group(
        Elem("ID", Attributes(), Group(Text(maxTaskId.toString))),
        Elem("OutlineNumber", Attributes(), Group(Text(outline))),
        Elem("Name", Attributes(), Group(Text(name))),
        Elem("Note", Attributes()),
        Elem("BaseStartDate", Attributes()),
        Elem("BaseFinishDate", Attributes()),
        Elem("BaseDuration", Attributes()),
        Elem("BaseDurationTimeUnit", Attributes()),
        Elem("ActualStartDate", Attributes()),
        Elem("ActualFinishDate", Attributes()),
        Elem("ActualDuration", Attributes()),
        Elem("ActualCost", Attributes()),
        Elem("Cost1", Attributes()),
        Elem("RecalcBase1", Attributes()),
        Elem("RecalcBase2", Attributes()),
        Elem("IsMilestone", Attributes()),
        Elem("Complete", Attributes()),
        Elem("IsHaveDeadline", Attributes()),
        Elem("SchedulingType", Attributes()),
        Elem("IsEffortDriven", Attributes()),
        Elem("Priority", Attributes()),
        Elem("MarkedByUser", Attributes()),
        Elem("StyleProject", Attributes()),
        Elem("ResourceAssignments", Attributes()),
        Elem("Callouts", Attributes()),
        Elem("rtfName", Attributes()),
        Elem("rtfNote", Attributes()),
        Elem("Objective", Attributes()),
        Elem("BranchPos", Attributes()),
        Elem("ValidatedByProject", Attributes()),
        Elem("Hyperlinks", Attributes(), Group.fromSeq(url.map({ s => Elem("Hyperlink", Attributes("xlink:href" -> s, "xlink:type" -> "simple")) }).toSeq) )
      ))
    }

    val newProject: Elem =
      if (!newTasks.isEmpty) {
        val groupedNewTasks = {
          newTasks groupBy (_.idList) groupBy (_._2.map(_.idBoard).head)
        }

        /*project.copy( children = project.children ++ Group.fromSeq(
          (groupedNewTasks.map { a =>
            List(newTaskXml(maxProjectOutlineId.toString, boards(a._1))) +
            boardTasks.map({ t => maxProjectOutlineId+=1; newTaskXml(maxProjectOutlineId.toString, a._2).toGroup }).flatten
          }).flatten)*/

        project.copy( children = project.children ++

          Seq(newTaskXml("1", "GGGG")) ++

          groupedNewTasks.map{ b =>
            maxProjectOutlineId+=1;
            val lst = List[Elem](newTaskXml(maxProjectOutlineId.toString, boards(b._1)))
            var lstIndent = 0;
            (lst ++
              (b._2.map { l =>
                lstIndent += 1;
                val lstIndentStr = maxProjectOutlineId.toString + "." + lstIndent.toString

                var tskIndent = 0;
                val lst2 = List[Elem](newTaskXml(maxProjectOutlineId.toString + "." + lstIndent.toString, lists(l._1)))

                (lst2 ++
                  (l._2.map { t =>
                    tskIndent += 1;
                    newTaskXml(lstIndentStr + "." + tskIndent.toString, t.name, Some(t.shortUrl))
                  })
                )
              }).flatten
            )
          }.flatten
          /*++
          newTasks.map{
            t => maxProjectOutlineId+=1;
            newTaskXml(maxProjectOutlineId.toString, genTaskName(t), Some(t.shortUrl))
          }*/
        )
      }
      else
        project.copy()


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

    val newProjectUpdated = (projects \ "Project").updated(0, newProject)
    val newXml = newProjectUpdated.unselect.unselect

    newXml.head.asInstanceOf[Elem].writeTo(writer)
    //XML.write(writer, newXml, "UTF-8", false, null)
    writer.flush()
  }
}