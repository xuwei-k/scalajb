package com.github.xuwei_k.scalajb

import scalaz.\/

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
