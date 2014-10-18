package com.github.xuwei_k.scalajb

import argonaut.CodecJson
import com.github.xuwei_k.scalajb.Scalajb.FIELD_DEF

sealed abstract class JsonLib(val value: String){
  def instance(clazz: CLAZZ, isImplicit: Boolean): String
}

object JsonLib {

  private[this] val quote: String => String =
    "\"" + _ + "\""

  private def implicitMod(isImplicit: Boolean): String =
    if(isImplicit) "implicit" else ""

  case object Argonaut extends JsonLib("argonaut"){
    def instance(clazz: CLAZZ, isImplicit: Boolean): String = {
s"""  ${implicitMod(isImplicit)} val ${clazz.className}CodecJson: CodecJson[${clazz.classNameUpper}] =
    CodecJson.casecodec${clazz.fields.size}(apply, unapply)(
""" + clazz.fields.map(_._1).map(quote).mkString("      ", ",\n      ", "\n    )")
    }
  }

  case object Play extends JsonLib("play"){
    private val f: FIELD_DEF => String = { case (key, tpe) =>
      s"""(__ \\ "$key").format[$tpe]"""
    }

    def instance(clazz: CLAZZ, isImplicit: Boolean): String = {
      val valdef = s"""${implicitMod(isImplicit)} val ${clazz.className}Format: OFormat[${clazz.classNameUpper}]"""
      val inmap = s"""${clazz.classNameUpper}.apply _, Function.unlift(${clazz.classNameUpper}.unapply)"""

      if (clazz.fields.length == 1) {
        s"""  $valdef = ${f(clazz.fields.head)}.inmap($inmap)"""
      } else { s"""
  $valdef = (
    ${clazz.fields.map(f).mkString(" and\n    ")}
  )($inmap)
"""
      }
    }
  }

  val all: Set[JsonLib] = Set(Argonaut, Play)
  val map: Map[String, JsonLib] = all.map(x => x.value -> x)(collection.breakOut)

  implicit val instance: CodecJson[JsonLib] =
    Scalajb.codecJsonFromStringEnum(JsonLib.map, _.value, _ + " is not valid json library name")

  def objectDef(clazz: CLAZZ, libs: Set[JsonLib], isImplicit: Boolean): String = s"""
object ${clazz.classNameUpper} {

${libs.map(_.instance(clazz, isImplicit)).mkString("\n\n")}

}
"""
}

