package com.github.xuwei_k.scalajb

import java.lang.reflect.InvocationTargetException

import argonaut.{DecodeResult, DecodeJson}
import com.typesafe.config.ConfigOrigin
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scalaz.{-\/, \/-, \/}

object DurationInstances {
  /**
   * [[https://github.com/typesafehub/config/blob/v1.2.1/config/src/main/java/com/typesafe/config/impl/SimpleConfig.java#L479-L552]]
   */
  private[this] val parseDurationMethod = {
    val clazz = Class.forName("com.typesafe.config.impl.SimpleConfig")
    val method = clazz.getMethod("parseDuration", classOf[String], classOf[ConfigOrigin], classOf[String])
    method.setAccessible(true)
    method
  }

  private[this] val emptyConfigOrigin = new ConfigOrigin {
    override def resource() = ""
    override def url() = null
    override def filename() = "<empty>"
    override def description() = "emptyConfigOrigin"
    override def comments() = java.util.Collections.emptyList()
    override def lineNumber() = -1
  }

  private[this] def parseDuration(input: String, path: String): Throwable \/ Long = {
    try {
      \/-(parseDurationMethod.invoke(null, input, emptyConfigOrigin, path).asInstanceOf[Long])
    } catch {
      case e: InvocationTargetException =>
        val cause = e.getCause
        -\/(
          if(cause != null) cause
          else e
        )
    }
  }

  implicit val durationDecodeJson: DecodeJson[Duration] =
    DecodeJson[Duration]( cursor =>
      implicitly[DecodeJson[String]].apply(cursor).flatMap( string =>
        parseDuration(string, cursor.history.toString) match {
          case \/-(value) =>
            val d = Duration.apply(value, TimeUnit.NANOSECONDS)
            DecodeResult.ok(d)
          case -\/(e) =>
            DecodeResult.fail(e.getMessage, cursor.history)
        }
      )
    )
}
