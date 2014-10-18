package com.github.xuwei_k.scalajb

import argonaut._
import scala.io.Source
import scalaz.{-\/, \/-}

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

  // TODO should not throw error
  def fromJSON(json: String, distinct: Boolean, topObjectName: Option[String]) =
    fromJValue(JsonParser.parse(json).fold(sys.error, identity), distinct, topObjectName)

  def fromHOCON(json: String, distinct: Boolean, topObjectName: Option[String]) =
    fromJSON(hocon2jsonString(json), distinct, topObjectName)

  def fromJValue(json: Json, distinct: Boolean, topObjectName: Option[String]) =
    objects(convert(json), distinct, topObjectName.getOrElse(unknownClassName))

  import com.github.xuwei_k.scalajb.Types._

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
    classes.head.copy(fieldSet = require ++ optionalFields)
  }

  final val unknownClassName = "Unknown"

  def objects(v: Value, d: Boolean, name: String = unknownClassName, depth: Int = 0): Set[CLAZZ] = {
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

  def escapeScala(word: String) = if(reserved(word)) "`" + word + "`" else word
  def escapeJava(word: String) = if(javaReserved(word)) "_" + word else word

  def hocon2jsonString(hocon: String): String = {
    import com.typesafe.config._
    ConfigFactory.parseString(hocon).root.render(ConfigRenderOptions.concise)
  }
}

