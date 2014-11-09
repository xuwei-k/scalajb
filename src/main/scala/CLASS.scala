package com.github.xuwei_k.scalajb

import scalaz.\/

object CLASS{
  sealed abstract class T{
    def javaStr: String = toString
    def isOpt: Boolean = false
    def optUnknown: Boolean = false
    def isUnknown: Boolean = false
  }
  case object Unknown  extends T{
    override def isUnknown: Boolean = true
  }
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
  final case class Opt(t: T) extends T{
    override val toString = "Option[" + t  + "]"
    override val javaStr = "Option<" + t + ">"
    override def isOpt: Boolean = true
    override def optUnknown: Boolean = t.isUnknown
  }
  final case class Obj(name: String) extends T{
    override val toString = Scalajb.toCamelUpper(name)
  }
  final case class Array(name: Set[String] \/ String) extends T{
    override val toString = {
      name.fold(types =>
        if(types.isEmpty) "List[Unknown]"
        else types.mkString(" or "),
        identity
      )
    }
  }
}
