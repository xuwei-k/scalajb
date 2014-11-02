package com.github.xuwei_k.scalajb

import argonaut.CodecJson

import scalaz.{\/, \/-}

final case class Param (
  private val jsonString: String,
  topObjectName: Option[String],
  private val _distinct: Option[Boolean],
  private val jsonLibs: Option[JsonLib OrElse Set[JsonLib]],
  private val _lang: Option[Lang],
  private val _isImplicit: Option[Boolean],
  private val _hocon: Option[Boolean],
  private val _comment: Option[Boolean]
) {
  def json: String \/ String = {
    if(hocon) {
      Scalajb.hocon2jsonString(jsonString).leftMap(error =>
        error.toString + error.getStackTrace.mkString("\n\n", "\n", "")
      )
    } else \/-(jsonString)
  }
  def distinct: Boolean = _distinct.getOrElse(Param.DISTINCT.default)
  def lang: Lang = _lang.getOrElse(Param.Default.lang)
  def isImplicit: Boolean = _isImplicit.getOrElse(Param.IMPLICIT.default)
  def jsonLibrary: Set[JsonLib] = jsonLibs.map(_.run.fold(Set.apply(_), identity)).getOrElse(Set.empty)
  def comment: Boolean = _comment.getOrElse(Param.COMMENT.default)
  private val hocon: Boolean = _hocon.getOrElse(Param.HOCON.default)
}

object Param {
  object Default {
    final val lang = Lang.SCALA
  }

  final val JSON = "json"
  final val TOP_OBJECT_NAME = "top_object_name"
  final val JSON_LIBRARY = "json_library"
  final val LANG = "lang"

  val IMPLICIT = BoolParam("implicit", true)
  val DISTINCT = BoolParam("distinct", true)
  val HOCON = BoolParam("hocon", false)
  val COMMENT = BoolParam("comment", true)

  implicit val instance: CodecJson[Param] =
    CodecJson.casecodec8(apply, unapply)(
      JSON,
      TOP_OBJECT_NAME,
      DISTINCT.key,
      JSON_LIBRARY,
      LANG,
      IMPLICIT.key,
      HOCON.key,
      COMMENT.key
    )
}
