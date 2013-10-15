
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
                  case x => println(x.toString); x.printStackTrace()
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

  def xmlTaskToBoardTask(xmlTask: Elem, tasks: Map[String, Task]) = {

    val links = (xmlTask \ "Hyperlinks" \ "Hyperlink").flatMap(_.attr(QName("xlink","href")))
    val text = textOf((xmlTask \ "Name"))
    val urlLinks = !links.isEmpty && links.head.startsWith("https://trello.com/c/")
    if (urlLinks || text.contains("https://trello.com/c/")) {
      val url = if (urlLinks) links.head else {
        val start = text.indexOf("https://trello.com/c/")
        val end = text.indexWhere(_.isSpaceChar, start)
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

    val mindmapTasks = (projects \ "Project" \ "Task").map(t => xmlTaskToBoardTask(t, boardTasks)).flatten

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

    def updateProjectTask(elem: Elem): Elem = {
      val t = xmlTaskToBoardTask(elem, tasks)
      if (t.isDefined) {
        val title = elem \ "Name"
        val newTitle : Elem = <Name>{genTaskName(t.get)}</Name>.convert

        val hyperlinks = title.updated(0, newTitle).unselect \ "Hyperlinks"
        val newHyperlinks = Elem("Hyperlinks", Attributes(), Group.fromSeq(t.map({ s => Elem("Hyperlink", Attributes("xlink:href" -> s.shortUrl, "xlink:type" -> "simple")) }).toSeq) )

        hyperlinks.updated(0, newHyperlinks).unselect.head.asInstanceOf[Elem]
      }
      else
        elem
    }

    def updateProjectNodes(elements: Group[Node]) : Group[Node] = {
      elements.map(a => { a match {
        case e @ Elem(_, "Task", _, _, _) => updateProjectTask(e)
        case _ => a
      }})
    }

    def newTaskXml(outline: Seq[Long], name: String, url: Option[String] = None) : Elem ={
      maxTaskId += 1
      Elem("Task", Attributes(),Group(
        Elem("ID", Attributes(), Group(Text(maxTaskId.toString))),
        Elem("OutlineNumber", Attributes(), Group(Text(outline.mkString(".")))),
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

        val outline = List(maxProjectOutlineId)

        project.copy( children = updateProjectNodes(project.children) ++
          Seq(newTaskXml(outline, "Inbox - " + DateTime.now.toDateTimeISO)) ++ {
            var boardLevel: Long = 0;
  
            groupedNewTasks.map{ b =>
              boardLevel+=1;
              val lst = List[Elem](newTaskXml(outline ++ List(boardLevel), boards(b._1)))
              var listLevel: Long = 0;
              (lst ++
                (b._2.map { l =>
                  listLevel += 1;  
                  var taskLevel: Long = 0;
                  val lst2 = List[Elem](newTaskXml(outline ++ List(boardLevel, listLevel), lists(l._1)))
  
                  (lst2 ++
                    (l._2.map { t =>
                      taskLevel += 1;
                      newTaskXml(outline ++ List(boardLevel, listLevel, taskLevel), genTaskName(t), Some(t.shortUrl))
                    })
                  )
                }).flatten
              )
            }.flatten
          }
        )
      }
      else
        project.copy(children = updateProjectNodes(project.children))

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