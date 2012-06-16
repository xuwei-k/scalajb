import sbt._,Keys._

object build extends Build{

  val UF = "0.6.3"

  lazy val root = Project(
    "scalajb",
    file(".")
  ).settings(
    sbtappengine.Plugin.webSettings ++ Seq(
      organization := "com.github.xuwei-k",
      licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "2.9.2",
      libraryDependencies ++= Seq(
        "net.liftweb" %% "lift-json" % "2.4",
        "net.databinder" %% "unfiltered-filter" % UF,
        "net.databinder" %% "unfiltered-spec" % UF % "test",
        "javax.servlet" % "servlet-api" % "2.3" % "provided",
        "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "container"
      ),
      resolvers ++= Seq(
       "https://repository.jboss.org/nexus/content/groups/public/",
       "http://xuwei-k.github.com/mvn"
      ).map{u => u at u}
    ) :_*
  )

}

