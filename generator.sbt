val baseURL = "https://raw.githubusercontent.com/unfiltered/unfiltered/v0.8.1/specs2/src/main/scala/"
val unfilteredSpecs2urls = List(
  "jetty/Served.scala", "Hosted.scala"
)
val unfilteredSpecs2Files = SettingKey[Seq[(String, Seq[String])]]("unfiltered-specs2-files")

unfilteredSpecs2Files := {
  unfilteredSpecs2urls.map{ name =>
    val sourceURL = baseURL + name
    println(s"downloading from $sourceURL")
    val lines = IO.readLinesURL(url(sourceURL))
    println(s"download finished $sourceURL")
    name.split('/').last -> lines
  }
}

sourceGenerators in Test += task{
  unfilteredSpecs2Files.value.map{ case (name, contents) =>
    val f = (sourceManaged in Test).value / name
    IO.writeLines(f, contents)
    f
  }
}

