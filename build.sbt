name := "trello-mindmap"

version := "1.0-SNAPSHOT"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies += "play" % "play_2.10" % "2.1.5"

libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

libraryDependencies += "org.scalesxml" %% "scales-xml" % "0.4.5"

libraryDependencies += "no.arktekk" %% "anti-xml" % "0.5.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

