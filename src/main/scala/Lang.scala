package com.github.xuwei_k.scalajb

import argonaut.CodecJson

sealed abstract class Lang(val name: String)
object Lang {
  val all: Set[Lang] = Set(JAVA, SCALA)
  val map: Map[String, Lang] = all.map(x => x.name -> x)(collection.breakOut)
  case object JAVA  extends Lang("java")
  case object SCALA extends Lang("scala")

  implicit val instance: CodecJson[Lang] =
    Scalajb.codecJsonFromStringEnum(map, _.name, _ + " is not valid lang name")
}
