name := "scalajb"

organization := "com.github.xuwei-k"

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  Nil
)

// https://github.com/unfiltered/unfiltered/blob/v0.8.1/project/common.scala#L6
// https://github.com/unfiltered/unfiltered/blob/v0.8.2/project/common.scala#L6
// https://code.google.com/p/googleappengine/issues/detail?id=3091
val unfilteredVersion = "0.8.1"
val scalazVersion = "7.1.0"

libraryDependencies ++= (
  ("com.typesafe" % "config" % "1.2.1") ::
  ("com.github.xuwei-k" %% "play-twenty-three-generator" % "0.1.2") ::
  ("org.scalaz" %% "scalaz-core" % scalazVersion) ::
  ("org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test") ::
  ("io.argonaut" %% "argonaut" % "6.1-M4") ::
  ("net.databinder" %% "unfiltered-filter" % unfilteredVersion) ::
  ("net.databinder" %% "unfiltered-jetty" % unfilteredVersion % "test") ::
  ("net.databinder" %% "dispatch-http" % "0.8.10" % "test") ::
  ("org.specs2" %% "specs2-core" % "2.4.6" % "test") ::
  ("org.scalaj" %% "scalaj-http" % "0.3.16" % "test") ::
  ("javax.servlet" % "servlet-api" % "2.3" % "provided") ::
  ("org.eclipse.jetty" % "jetty-webapp" % "8.1.16.v20140903" % "container") ::
  Nil
)
