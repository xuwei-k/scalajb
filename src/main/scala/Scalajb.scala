package com.github.xuwei_k.scalajb

import net.liftweb.json._
import scala.io.Source

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


  def fromURL(url:String,distinct:Boolean) = fromJSON(Source.fromURL(url).mkString,distinct:Boolean)

  def fromJSON(json:String,distinct:Boolean) = fromJValue(parse(json),distinct)

  def fromJValue(json:JValue,distinct:Boolean) = objects(json,distinct)

  def type2t(f:JField):FIELD_DEF = {
    val k = f._1
    val v =
    f._2 match {
      case JNull | JNothing  => CLASS.Unknown
      case JString(_)        => CLASS.String
      case JDouble(_)        => CLASS.Double
      case JInt(_)           => CLASS.Long
      case JBool(_)          => CLASS.Boolean
      case JObject(_)        => CLASS.Obj(k)
      case JArray(arr)       =>
        val r = arr.map(o => type2t((k,o))).toSet
        if(r.size == 1){
          CLASS.Array(Right(k))
        }else{
          CLASS.Array(Left(r.map(_._1)))
        }
    }
    k -> v
  }

  def distinct(classes:Set[CLAZZ]):CLAZZ = {
    val all = classes.flatMap{_.fields.toSet}
    val optionals = classes.flatMap{c =>
      all -- c.fields
    }
    val require = all -- optionals
    val optionalFields = optionals.map{case (name,t) => name -> CLASS.Opt(t)}
    classes.head.copy(fields = require ++ optionalFields)
  }

  def objects(v:JValue,d:Boolean,name:String = "Unknown",depth:Int = 0):Set[CLAZZ] = {
    v match{
      case JNull | JNothing  => Set.empty
      case JString(_)        => Set.empty
      case JDouble(_)        => Set.empty
      case JInt(_)           => Set.empty
      case JBool(_)          => Set.empty
      case JObject(obj)      => {
        val children = obj.flatMap{case (a,b) => objects(b,d,a,depth + 1)}.toSet
        children + CLAZZ(name,obj.map{type2t}.toSet,depth)
      }
      case JArray(obj)  =>
        val children = obj.flatMap(v => objects(v,d)).toSet
        if(d){
          val (other,oneOrZero) = children.groupBy(_.depth).map(_._2).partition{_.size > 1}
          other.map(distinct).toSet ++ oneOrZero.flatten
        }else{
          children
        }
    }
  }

  type FIELD_DEF = (String,CLASS.T)

  case class CLAZZ(name:String,fields:Set[FIELD_DEF],depth:Int){
    val className = name.head.toUpper + name.tail

    override def toString = scalaStr

    // TODO when over 23 fields. create abstract class or trait instead of case class ?
    def scalaStr:String = {
      val _fields = fields.map{case (k,v) => escapeScala(k) -> v}
      val max = _fields.map(_._1.size).max
      val n = name.head.toUpper + name.tail
      _fields.map{
        case (k,t) =>
          val indent = " " * (max - k.size)
        "  " + k + indent + " :" + t
      }.mkString("case class " + n + "(\n",",\n","\n)\n")
    }

    def str(lang:Lang) = lang match{
      case JAVA  => javaStr()
      case SCALA => scalaStr
    }

    def javaStr(indentStr:String = "  "):String = {
      def i(indentLevel:Int) = indentStr * indentLevel
      val _fields = fields.map{case (k,v) => escapeJava(k) -> v}

      Iterator(
        "public class " + className + "{",
          i(1) + "public " + className + "(",
          i(2) + _fields.map{case (k,t) =>
            "final " + t.javaStr + " " + k
          }.mkString(","),
            i(1) + "){",
          _fields.map{case (k,t) =>
            i(2) + "this." + k + " = " + k + ";"
          }.mkString("","\n","\n" + i(1) + "}\n"),
          _fields.map{case (k,t) =>
            i(1) + "public final " + t.javaStr + " " + k + ";"
          }.mkString("\n"),
        "}\n"
      ).mkString("\n")
    }
  }

  def escapeScala(word:String) = if(reserved(word)) "`" + word + "`" else word
  def escapeJava(word:String) = if(javaReserved(word)) "_" + word else word

}

object CLASS{
  sealed abstract class T{
    def javaStr:String = toString
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
  case class  Opt(t:T) extends T{
    override val toString = "Option[" + t  + "]"
    override val javaStr = "Option<" + t + ">"
  }
  case class  Obj(name:String) extends T{
    override val toString = name.head.toUpper + name.tail
  }
  case class  Array(name:Either[Set[String],String]) extends T{
    override val toString = {
      name.fold(_.mkString(" or "),identity)
    }
  }
}
