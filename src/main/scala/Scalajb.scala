package com.github.xuwei_k.scalajb

import argonaut._
import scala.io.Source
import scalaz.{-\/, \/-, \/}

object Scalajb{
  val reserved = Set(
    "abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "false", "final", "finally",
    "for", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield")

  val javaReserved = Set(
    "byte", "char", "short", "int", "long", "float", "double",
    "boolean", "true" , "false", "void", "if" , "else" , "switch" , "case" , "default",
    "for" , "while" , "do" , "continue" , "break" , "return", "package" , "import" ,
    "class" , "interface" , "extends" , "implements" , "this" , "super" , "new",
    "null" , "instanceof" , "public" , "protected" , "private" , "final" ,
    "static" , "abstract" , "native" , "synchronized" , "try" , "catch" , "finally",
    "strictfp", "throw" ,"assert" , "enum" ,"const" ,"goto","throws", "transient","volatile"
  )

  def fromURL(url: String, distinct: Boolean) =
    fromJSON(Source.fromURL(url).mkString, distinct: Boolean)

  // TODO should not throw error
  def fromJSON(json: String, distinct: Boolean) =
    fromJValue(JsonParser.parse(json).fold(sys.error, identity), distinct)

  def fromJValue(json: Json, distinct: Boolean) = objects(convert(json), distinct)

  import Types._

  def convert(j: Json): Value = j.fold[Types.Value](
    jsonNull = NULL,
    jsonBool = BOOL,
    jsonNumber = { value =>
      val n = value.toLong
      if(value.toLong == value){
        INT(n)
      }else{
        DOUBLE(value)
      }
    },
    jsonString = STRING,
    jsonArray = array => ARRAY(array.map(convert)),
    jsonObject = obj => OBJ(obj.toList.map{ case (k, v) => k -> convert(v)})
  )

  def type2t(f: Field): FIELD_DEF = {
    val k = f._1
    val v =
    f._2 match {
      case NULL        => CLASS.Unknown
      case STRING(s)   => CLASS.String
      case DOUBLE(num) => CLASS.Double
      case INT(num)    => CLASS.Long
      case BOOL(value) => CLASS.Boolean
      case OBJ(obj)    => CLASS.Obj(k)
      case ARRAY(arr)  =>
        val r = arr.map(o => type2t((k, o))).toSet
        if(r.size == 1){
          CLASS.Array(\/-(k))
        }else{
          CLASS.Array(-\/(r.map(_._1)))
        }
    }
    k -> v
  }

  def distinct(classes: Set[CLAZZ]): CLAZZ = {
    val all = classes.flatMap{_.fields.toSet}
    val optionals = classes.flatMap{c =>
      all -- c.fields
    }
    val require = all -- optionals
    val optionalFields = optionals.map{case (name, t) => name -> CLASS.Opt(t)}
    classes.head.copy(fields = require ++ optionalFields)
  }

  def objects(v: Value, d: Boolean, name: String = "Unknown", depth: Int = 0): Set[CLAZZ] = {
    v match{
      case NULL        => Set.empty
      case STRING(_)   => Set.empty
      case DOUBLE(_)   => Set.empty
      case INT(_)      => Set.empty
      case BOOL(_)     => Set.empty
      case OBJ(obj)    => {
        val children = obj.flatMap{case (a, b) => objects(b, d, a, depth + 1)}.toSet
        children + CLAZZ(name, obj.map{type2t}.toSet, depth)
      }
      case ARRAY(obj)  =>
        val children = obj.flatMap(v => objects(v, d)).toSet
        if(d){
          val (other, oneOrZero) = children.groupBy(_.depth).map(_._2).partition{_.size > 1}
          other.map(distinct).toSet ++ oneOrZero.flatten
        }else{
          children
        }
    }
  }

  type FIELD_DEF = (String, CLASS.T)

  case class CLAZZ(name: String, fields: Set[FIELD_DEF], depth: Int){
    val className = name.head.toUpper + name.tail

    override def toString = scalaStr

    // TODO when over 23 fields. create abstract class or trait instead of case class ?
    def scalaStr: String = {
      val _fields = fields.map{case (k, v) => escapeScala(k) -> v}
      val max = _fields.map(_._1.size).max
      val n = name.head.toUpper + name.tail
      _fields.map{
        case (k, t) =>
          val indent = " " * (max - k.size)
        "  " + k + indent + " :" + t
      }.mkString("final case class " + n + "(\n", ",\n", "\n)\n")
    }

    def str(lang: Lang) = lang match{
      case JAVA  => javaStr()
      case SCALA => scalaStr
    }

    def javaStr(indentStr: String = "  "): String = {
      def i(indentLevel: Int) = indentStr * indentLevel
      val _fields = fields.map{case (k, v) => escapeJava(k) -> v}

      Iterator(
        "public class " + className + "{",
          i(1) + "public " + className + "(",
          i(2) + _fields.map{case (k, t) =>
            "final " + t.javaStr + " " + k
          }.mkString(","),
            i(1) + "){",
          _fields.map{case (k, t) =>
            i(2) + "this." + k + " = " + k + ";"
          }.mkString("", "\n", "\n" + i(1) + "}\n"),
          _fields.map{case (k, t) =>
            i(1) + "public final " + t.javaStr + " " + k + ";"
          }.mkString("\n"),
        "}\n"
      ).mkString("\n")
    }
  }

  def escapeScala(word: String) = if(reserved(word)) "`" + word + "`" else word
  def escapeJava(word: String) = if(javaReserved(word)) "_" + word else word

}

object Types{
  sealed abstract class Value
  case object NULL extends Value
  case class STRING(s: String) extends Value
  case class DOUBLE(num: Double) extends Value
  case class INT(num: Long) extends Value
  case class BOOL(value: Boolean) extends Value
  type Field = (String, Value)
  case class OBJ(obj: List[Field]) extends Value
  case class ARRAY(arr: List[Value]) extends Value
}

object CLASS{
  sealed abstract class T{
    def javaStr: String = toString
  }
  case object Unknown  extends T
  case object String   extends T
  case object Double   extends T{
    override val javaStr = "double"
  }
  case object Long     extends T{
    override val javaStr = "long"
  }
  case object Boolean  extends T{
    override val javaStr = "boolean"
  }
  case class  Opt(t: T) extends T{
    override val toString = "Option[" + t  + "]"
    override val javaStr = "Option<" + t + ">"
  }
  case class  Obj(name: String) extends T{
    override val toString = name.head.toUpper + name.tail
  }
  case class  Array(name: Set[String] \/ String) extends T{
    override val toString = {
      name.fold(_.mkString(" or "), identity)
    }
  }
}
