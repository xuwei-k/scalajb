package com.github.xuwei_k.scalajb

import com.github.xuwei_k.scalajb.Scalajb.FIELD_DEF

sealed abstract class JsonLib(val value: String){
  def instance(clazz: CLAZZ): String
}

object JsonLib {
  private[this] val quote: String => String =
    "\"" + _ + "\""

  case object Argonaut extends JsonLib("argonaut"){
    def instance(clazz: CLAZZ): String = s"""
  val ${clazz.name}CodecJson: CodecJson[${clazz.name}] =
    CodecJson.casecodec${clazz.fields.size}(apply, unapply)(
""" + clazz.fields.map(_._1).map(quote).mkString("      ", ",\n      ", "\n    )")
  }

  case object Play extends JsonLib("play"){
    private val f: FIELD_DEF => String = { case (key, tpe) =>
      s"""(__ \\ "$key").format[$tpe]"""
    }

    def instance(clazz: CLAZZ): String = s"""
  val ${clazz.name}Format: Format[${clazz.name}] = (
    ${clazz.fields.map(f).mkString(" and\n    ")}
  )(${clazz.name}.apply _, Function.unlift(${clazz.name}.unapply))
"""
  }

  val all: Set[JsonLib] = Set(Argonaut, Play)
  val map: Map[String, JsonLib] = all.map(x => x.value -> x)(collection.breakOut)

  def objectDef(clazz: CLAZZ, libs: Set[JsonLib]): String = s"""
object ${clazz.name} {

  ${libs.map(_.instance(clazz)).mkString("\n\n")}

}
"""
}

