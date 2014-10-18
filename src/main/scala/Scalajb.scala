package com.github.xuwei_k.scalajb

import argonaut._
import com.typesafe.config.{ConfigRenderOptions, ConfigFactory, ConfigException}
import scalaz.{\/, -\/, \/-}

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

  def run(jsonString: String, topObjectName: Option[String], distinct: Boolean, libs: Set[JsonLib], lang: Lang, isImplicit: Boolean): String \/ String =
    Scalajb.fromJSON(jsonString, distinct, topObjectName).map(classes =>
      classes2string(classes, lang, libs, isImplicit)
    )

  def classes2string(classes: Set[CLAZZ], lang: Lang, libs: Set[JsonLib], isImplicit: Boolean): String =
    classes.toSeq.sortBy(_.depth).map{ clazz =>
      clazz.str(lang) + {
        if(lang == Lang.SCALA) JsonLib.objectDef(clazz, libs, isImplicit)
        else ""
      }
    }.mkString("\n\n")

  def fromJSON(json: String, distinct: Boolean, topObjectName: Option[String]): String \/ Set[CLAZZ] =
    JsonParser.parse(json).map(jsonObj =>
      fromJValue(jsonObj, distinct, topObjectName)
    )

  def fromJValue(json: Json, distinct: Boolean, topObjectName: Option[String]): Set[CLAZZ] =
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


  def toCamelUpper(word: String): String = {
    toCamel(word).toList match {
      case h :: t => (h.toUpper :: t).mkString
      case x => word
    }
  }

  def toCamel(word: String): String = {
    def function(words: Array[String]): String = {
      words.filter(_.nonEmpty).toList match {
        case head :: tail =>
          head :: tail.map{_.toList match{
            case h :: t => (h.toUpper :: t).mkString
            case x => x.mkString
          }}
        case other =>
          other
      }
    }.mkString

    function(function(word.split('-')).split('_'))
  }

  def escapeScala(word: String) = if(reserved(word)) "`" + word + "`" else toCamel(word)
  def escapeJava(word: String) = if(javaReserved(word)) "_" + word else toCamel(word)

  def hocon2jsonString(hocon: String): ConfigException \/ String =
    \/.fromTryCatchThrowable[String, ConfigException] {
      ConfigFactory.parseString(hocon).root.render(ConfigRenderOptions.concise)
    }
}

