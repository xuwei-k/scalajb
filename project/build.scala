import sbt._,Keys._

object build extends Build{

  val UF = "0.7.1"

  lazy val root = Project(
    "scalajb",
    file(".")
  ).settings(
    sbtappengine.Plugin.webSettings ++ Seq(
      organization := "com.github.xuwei-k",
      licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "2.10.4-RC3",
      scalacOptions ++= Seq("-deprecation", "-unchecked"),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0.5",
        "org.json4s" %% "json4s-native" % "3.2.7",
        "net.databinder" %% "unfiltered-filter" % UF,
        "net.databinder" %% "unfiltered-spec" % UF % "test",
        "org.scalaj" %% "scalaj-http" % "0.3.14" % "test",
        "javax.servlet" % "servlet-api" % "2.3" % "provided",
        "org.eclipse.jetty" % "jetty-webapp" % "8.1.13.v20130916" % "container"
      ),
      initialCommands in console := {
        Iterator(
          "com.github.xuwei_k.scalajb", "org.json4s"
        ).map{
          "import " + _ + "._;\n"
        }.mkString
      },
      resolvers ++= Seq(
        "jboss" at "https://repository.jboss.org/nexus/content/groups/public/"
      )
    ) :_*
  )

}

