package com.github.xuwei_k.scalajb

object Types{
  sealed abstract class Value
  case object NULL extends Value
  final case class STRING(s: String) extends Value
  final case class DOUBLE(num: Double) extends Value
  final case class INT(num: Long) extends Value
  final case class BOOL(value: Boolean) extends Value
  type Field = (String, Value)
  final case class OBJ(obj: List[Field]) extends Value
  final case class ARRAY(arr: List[Value]) extends Value
}
