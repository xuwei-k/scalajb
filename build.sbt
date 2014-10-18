name := "scalajb"

organization := "com.github.xuwei-k"

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

// https://github.com/unfiltered/unfiltered/blob/v0.8.1/project/common.scala#L6
// https://github.com/unfiltered/unfiltered/blob/v0.8.2/project/common.scala#L6
// https://code.google.com/p/googleappengine/issues/detail?id=3091
val UF = "0.8.1"

libraryDependencies ++= (
  ("org.scalaz" %% "scalaz-core" % "7.0.6") ::
  ("org.json4s" %% "json4s-native" % "3.2.10") ::
  ("net.databinder" %% "unfiltered-filter" % UF) ::
  ("net.databinder" %% "unfiltered-specs2" % UF % "test") ::
  ("org.scalaj" %% "scalaj-http" % "0.3.16" % "test") ::
  ("javax.servlet" % "servlet-api" % "2.3" % "provided") ::
  ("org.eclipse.jetty" % "jetty-webapp" % "8.1.16.v20140903" % "container") ::
  Nil
)

