package com.github.xuwei_k.scalajb

import com.github.xuwei_k.scalajb.Scalajb.FIELD_DEF

final case class CLAZZ(private val name: String, private val fieldSet: Set[FIELD_DEF], parent: Option[CLAZZ], depth: Int) {
  val className = Scalajb.toCamel(name)
  val classNameUpper = Scalajb.toCamelUpper(name)

  lazy val fields: Seq[FIELD_DEF] = fieldSet.groupBy(_._1).values.flatMap{ x =>
    val values = x.map(_._2)
    if(values.size > 1 && values.forall(_.isOpt) && values.exists(! _.optUnknown)){
      x.filterNot(_._2.optUnknown)
    }else x
  }.toList.sortBy(_._1)

  override def toString = scalaStr(true)

  private val allParents: List[CLAZZ] = parent.map(p => p :: p.allParents).getOrElse(Nil)

  // TODO when over 23 fields. create abstract class or trait instead of case class ?
  def scalaStr(enableComment: Boolean): String = {
    val comment = if(enableComment){
      allParents.reverse.map(_.name).mkString("// ",".", "\n")
    }else ""
    val _fields = fields.map { case (k, v) => Scalajb.escapeScala(k) -> v}
    val max = _fields.map(_._1.size).reduceOption(_ max _).getOrElse(0)
    _fields.map {
      case (k, t) =>
        val indent = " " * (max - k.size)
        "  " + k + indent + " :" + t
    }.mkString(comment + "final case class " + classNameUpper + "(\n", ",\n", "\n)\n")
  }

  def str(lang: Lang, comment: Boolean) = lang match {
    case Lang.JAVA => javaStr()
    case Lang.SCALA => scalaStr(comment)
  }

  def javaStr(indentStr: String = "  "): String = {
    def i(indentLevel: Int) = indentStr * indentLevel
    val _fields = fields.map { case (k, v) => Scalajb.escapeJava(k) -> v}

    Iterator(
      "public class " + classNameUpper + "{",
      i(1) + "public " + classNameUpper + "(",
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
