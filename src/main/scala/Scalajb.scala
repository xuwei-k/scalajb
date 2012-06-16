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

  // TODO
  val javaReserved = Set(
    "native","strictfp", "switch", "throws", "transient","volatile")

  def fromURL(url:String) = fromJSON(Source.fromURL(url).mkString)

  def fromJSON(json:String) = fromJValue(parse(json))

  def fromJValue(json:JValue) = objects(convert(json))

  import Types._

  def convert(j:JValue):Value = {
    j match {
      case JObject(obj)        => OBJ(obj.map{case JField(k,v) => k -> convert(v)})
      case JArray(arr)         => ARRAY(arr.map(convert))
      case JNothing | JNull    => NULL
      case JString(s)          => STRING(s)
      case JDouble(num)        => DOUBLE(num)
      case JInt(num)           => INT(num.longValue)
      case JBool(value)        => BOOL(value)
    }
  }

  def type2t(f:Field):FIELD_DEF = {
    val k = if(reserved(f._1)) "`" + f._1 + "`" else f._1
    val v =
    f._2 match {
      case NULL        => CLASS.Unknown
      case STRING(s)   => CLASS.String
      case DOUBLE(num) => CLASS.Double
      case INT(num)    => CLASS.Long
      case BOOL(value) => CLASS.Boolean
      case OBJ(obj)    => CLASS.Obj(k)
      case ARRAY(arr)  =>
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

  def objects(v:Value,name:String = "Unknown",depth:Int = 0):Set[CLAZZ] = {
    v match{
      case NULL        => Set.empty 
      case STRING(_)   => Set.empty
      case DOUBLE(_)   => Set.empty
      case INT(_)      => Set.empty
      case BOOL(_)     => Set.empty
      case OBJ(obj)    => {
        val children = obj.flatMap{case (a,b) => objects(b,a,depth + 1)}.toSet
        children + CLAZZ(name,obj.map{type2t}.toSet,depth) 
      }
      case ARRAY(obj)  => 
        val children = obj.flatMap(objects(_)).toSet
        val (other,oneOrZero) = children.groupBy(_.depth).map(_._2).partition{_.size > 1}
        other.map(distinct).toSet ++ oneOrZero.flatten
    }
  }

  type FIELD_DEF = (String,CLASS.T)

  case class CLAZZ(name:String,fields:Set[FIELD_DEF],depth:Int){
    // TODO when over 23 fields. create abstract class or trait instead of case class ?
    override lazy val toString = {
      val max = fields.map(_._1.size).max 
      val n = name.head.toUpper + name.tail
      fields.map{ 
        case (k,t) =>
          val indent = " " * (max - k.size)
        "  " + k + indent + " :" + t
      }.mkString("case class " + n + "(\n",",\n","\n)\n")
    }
  }
}

object Types{
  sealed abstract class Value
  case object NULL extends Value
  case class STRING(s: String) extends Value
  case class DOUBLE(num: Double) extends Value
  case class INT(num: Long) extends Value
  case class BOOL(value: Boolean) extends Value
  type Field = (String,Value)
  case class OBJ(obj: List[Field]) extends Value 
  case class ARRAY(arr: List[Value]) extends Value
}

object CLASS{
  sealed abstract class T
  case object Unknown  extends T
  case object String   extends T
  case object Double   extends T
  case object Long     extends T
  case object Boolean  extends T
  case class  Opt(t:T) extends T{
    override val toString = "Option[" + t  + "]"
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
