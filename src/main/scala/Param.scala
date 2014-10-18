package com.github.xuwei_k.scalajb

import argonaut.CodecJson

final case class Param (
  jsonString: String,
  topObjectName: Option[String],
  private val _distinct: Option[Boolean],
  private val jsonLibs: Option[Set[JsonLib]],
  private val _lang: Option[Lang],
  private val _isImplicit: Option[Boolean]
) {
  def distinct: Boolean = _distinct.getOrElse(Param.Default.distinct)
  def lang: Lang = _lang.getOrElse(Param.Default.lang)
  def isImplicit: Boolean = _isImplicit.getOrElse(Param.Default.isImplicit)
  def jsonLibrary: Set[JsonLib] = jsonLibs.getOrElse(Set.empty)
}

object Param {
  object Default {
    final val distinct = true
    final val lang = Lang.SCALA
    final val isImplicit = true
  }

  final val JSON = "json"
  final val TOP_OBJECT_NAME = "top_object_name"
  final val DISTINCT = "distinct"
  final val JSON_LIBRARY = "json_library"
  final val LANG = "lang"
  final val IMPLICIT = "implicit"

  implicit val instance: CodecJson[Param] =
    CodecJson.casecodec6(apply, unapply)(
      JSON,
      TOP_OBJECT_NAME,
      DISTINCT,
      JSON_LIBRARY,
      LANG,
      IMPLICIT
    )
}
