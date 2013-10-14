import com.typesafe.config.ConfigFactory
import java.io._
import java.util.zip._
import org.apache.commons.io.IOUtils
import org.joda.time.DateTime
import play.api.http.Status
import play.api.libs.json.{JsArray, JsValue, JsObject}
import play.api.libs.ws.WS
import scala.Some
import scala.xml._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
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
          if (e.getName() == "content.xml")
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

  def getAttributeText(node: Node, s: String): String = node.attributes.find(_.key == s) map {
    a => a.value.text
  } getOrElse ""

  def topicToTask(topic: Node, tasks: Map[String, Task]) = {
    val url = cleanUrl(getAttributeText(topic, "href"))
    if (url.startsWith("https://trello.com/c/")) {
      val id = ((topic \ "@id").text)
      val text = ((topic \ "title").text)
      val date = try {
        new DateTime(((topic \ "@timestamp").text).toLong)
      }
      catch {
        case _: Throwable => new DateTime()
      }

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

    val original = XML.load(in)

    val mindmapTasks = (original \\ "topic").map(topicToTask(_, boardTasks)).flatten
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

    object scope {

      def attributesMatch(a: Elem, b: Elem): Boolean =
        a.attributes.foldLeft(true)((r, i) => {
          val left = a.attribute(i.prefixedKey)
          val right = b.attribute(i.prefixedKey)
          r && left == right
        })

      def childrenMatch(a: Elem, b: Elem): Boolean =
        a.child.filter(_.isInstanceOf[Elem]).foldLeft(true)((r, i) => {
          val left = i.asInstanceOf[Elem]
          val right = b \ i.label
          if (!right.isEmpty) {
            val e = right.head.asInstanceOf[Elem]
            r && (left.label == e.label) && (left.text == e.text) && attributesMatch(left, e) && childrenMatch(left, e)
          }
          else
            false
        })

      def appendChildren(path: Seq[Elem], siblings: Seq[Node], appendNodes: Iterable[Elem]): Seq[Node] = {
        var found = false

        val seq =
          for (c <- siblings) yield
            if (c.isInstanceOf[Elem]
              && c.label == path.head.label
              && attributesMatch(path.head, c.asInstanceOf[Elem])
              && childrenMatch(path.head, c.asInstanceOf[Elem])
              && !found) {
              found = true
              val e = c.asInstanceOf[Elem]
              if (path.tail.isEmpty)
                e.copy(child = e.child ++ appendNodes)
              else
                e.copy(child = appendChildren(path.tail, e.child, appendNodes))
            }
            else
              c

        if (!found) {

          val revPath = path.reverse
          val inner = revPath.head.copy(child = revPath.head.child ++ appendNodes)
          val newNodeTree = revPath.tail.foldLeft(inner)((i, o) => o.copy(child = o.child ++ i))

          seq ++ newNodeTree
        }
        else
          seq
      }

      def modifyLabels(nodes: Seq[Node], t: Task): Seq[Node] = {

        def removeTrello(nodes: Seq[Node]): Seq[Node] = nodes.filter {
          n => !(n.isInstanceOf[Elem] && n.label == "label" && n.text.startsWith("trello-"))
        }

        val labels: Seq[Node] = List(
          boards.get(t.idBoard).toSeq.map(b => <label>
            {"trello-board-" + b}
          </label>),
          if (t.state == TaskState.Removed)
            <label>trello-removed</label>
          else
            NodeSeq.Empty
        ).flatten.map(xml.Utility.trim(_))

        var added = false
        val r = nodes.map {
          n =>
            if (n.isInstanceOf[Elem] && n.label == "labels") {
              added = true
              Some(n.asInstanceOf[Elem].copy(child = removeTrello(n.asInstanceOf[Elem].child) ++ labels))
            }
            else
              Some(n)
        }.flatten

        if (!added)
          r ++ <labels>
            {labels}
          </labels>
        else
          r
      }

      def modifyMarkers(nodes: Seq[Node], t: Task): Seq[Node] = {

        def removeTrello(nodes: Seq[Node]): Seq[Node] = nodes.filter {
          n => !(n.isInstanceOf[Elem] && n.label == "marker-ref" && (n \ "@marker-id").text == "symbol-wrong")
        }

        val markers = if (t.state == TaskState.Removed)
            <marker-ref marker-id="symbol-wrong"/>.map(xml.Utility.trim(_))
        else
          NodeSeq.Empty

        var added = false
        val r = nodes.map {
          n =>
            if (n.isInstanceOf[Elem] && n.label == "marker-refs") {
              added = true
              Some(n.asInstanceOf[Elem].copy(child = removeTrello(n.asInstanceOf[Elem].child) ++ markers))
            }
            else
              Some(n)
        }.flatten

        if (!added)
          r ++
            <marker-refs>
              {markers}
            </marker-refs>
        else
          r
      }

      def modifyTitle(nodes: Seq[Node], task: Task): Seq[Node] = {
        val list = lists.get(task.idList)
        if (list.isDefined) {
          nodes map {
            n =>
              if (n.isInstanceOf[Elem] && n.label == "title") {
                <title>
                  {task.name + " / " + list.get}
                </title>
              }
              else
                n
          }
        }
        else
          nodes
      }

      def modifyTaskChildren(nodes: Seq[Node], task: Task): Seq[Node] = modifyMarkers(nodes, task) //modifyTitle(nodes, task)//modifyMarkers(nodes/*modifyLabels(nodes, task)*/, task), task)
    }

    object AddNewTasks extends RewriteRule {

      override def transform(n: Node): Seq[Node] = n match {
        case e: Elem =>
          if (e.label == "xmap-content") {
            val newTasksOutline = groupedNewTasks.map {
              b =>
                <topic>
                <title>
                  {boards(b._1)}
                </title>
                // board name
                <children>
                  <topics type="attached">
                    {b._2 map {
                    l =>
                      <topic>//
                        <title>
                          {lists(l._1)}
                        </title>
                        // list name
                        <children>
                          <topics type="attached">
                            {l._2 map {
                            task =>
                              val t = <topic id={"trello-" + task.id} timestamp={task.date.toInstant.getMillis.toString} xlink:href={task.shortUrl}>
                                <title>
                                  {task.name}
                                </title>
                              </topic>
                              xml.Utility.trim(t.copy(child = scope.modifyTaskChildren(t.child, task)))
                          }}
                          </topics>
                        </children>
                      </topic>
                  }}
                  </topics>
                </children>
              </topic>
            }

            val path = List(
                <sheet/>,
                <topic/>,
                <children/>,
                <topics type="attached"/>,
              <topic>
                <title>[Inbox]</title>
              </topic>,
                <children/>,
                <topics type="attached"/>,
              <topic>
                <title>
                  {DateTime.now.toDateTimeISO}
                </title>
              </topic>,
                <children/>,
                <topics type="attached"/>
            )
            e.copy(child = scope.appendChildren(path, e.child, newTasksOutline))
          }
          else e
        case _ => n
      }
    }

    object UpdateExistingTasks extends RewriteRule {

      override def transform(n: Node): Seq[Node] = n match {
        case e: Elem =>
          if (e.label == "topic") {
            val et = topicToTask(e, tasks)
            if (et.isDefined) {
              val t = tasks(et.get.shortUrl)

              e.copy(child = scope.modifyTaskChildren(e.child, t))
            }
            else
              e
          }
          else
            e
        case _ => n
      }
    }

    try {
      val rule = new RuleTransformer(UpdateExistingTasks, AddNewTasks)
      val newXml = rule.apply(original)

      for (task <- newTasks) {
        logger.info("New task: " + boards(task.idBoard) + " / " + lists(task.idList) + " / " + task.shortUrl + " " + task.name)
      }

      for (task <- tasks.values.filter(t => t.state == TaskState.Removed)) {
        logger.info("Removed task: " + task.shortUrl + " " + task.name)
      }

      val writer = new OutputStreamWriter(out)
      XML.write(writer, newXml, "UTF-8", false, null)
      writer.flush()
    } catch {
      case x: Throwable => println(x)
    }
  }

  def cleanUrl(s: String) = if (s.startsWith("URL:")) s.substring(4).trim() else s
}