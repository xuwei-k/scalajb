package com.github.xuwei_k.scalajb

sealed abstract class JsonLib(val value: String){
}

object JsonLib {
  case object Argonaut extends JsonLib("argonaut")
  case object Play extends JsonLib("play")

  val all: Set[JsonLib] = Set(Argonaut, Play)
  val map: Map[String, JsonLib] = all.map(x => x.value -> x)(collection.breakOut)
}

