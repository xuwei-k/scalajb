package com.github.xuwei_k.scalajb

import com.github.xuwei_k.scalajb.Scalajb.FIELD_DEF

final case class CLAZZ(name: String, fields: Set[FIELD_DEF], depth: Int) {
  val className = name.head.toUpper + name.tail

  override def toString = scalaStr

  // TODO when over 23 fields. create abstract class or trait instead of case class ?
  def scalaStr: String = {
    val _fields = fields.map { case (k, v) => Scalajb.escapeScala(k) -> v}
    val max = _fields.map(_._1.size).max
    val n = name.head.toUpper + name.tail
    _fields.map {
      case (k, t) =>
        val indent = " " * (max - k.size)
        "  " + k + indent + " :" + t
    }.mkString("final case class " + n + "(\n", ",\n", "\n)\n")
  }

  def str(lang: Lang) = lang match {
    case Lang.JAVA => javaStr()
    case Lang.SCALA => scalaStr
  }

  def javaStr(indentStr: String = "  "): String = {
    def i(indentLevel: Int) = indentStr * indentLevel
    val _fields = fields.map { case (k, v) => Scalajb.escapeJava(k) -> v}

    Iterator(
      "public class " + className + "{",
      i(1) + "public " + className + "(",
      i(2) + _fields.map { case (k, t) =>
        "final " + t.javaStr + " " + k
      }.mkString(","),
      i(1) + "){",
      _fields.map { case (k, t) =>
        i(2) + "this." + k + " = " + k + ";"
      }.mkString("", "\n", "\n" + i(1) + "}\n"),
      _fields.map { case (k, t) =>
        i(1) + "public final " + t.javaStr + " " + k + ";"
      }.mkString("\n"),
      "}\n"
    ).mkString("\n")
  }
}
