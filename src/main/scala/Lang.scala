package com.github.xuwei_k.scalajb

sealed abstract class Lang
object Lang {
  case object JAVA  extends Lang
  case object SCALA extends Lang
}
