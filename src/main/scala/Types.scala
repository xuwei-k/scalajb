package com.github.xuwei_k.scalajb

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
