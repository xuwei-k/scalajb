package com.github.xuwei_k.scalajb

import argonaut.{DecodeResult, DecodeJson, EncodeJson, CodecJson}
import play.twentythree.{Param => _, _}

object TwentyThree {

  def SeqString(key: String): NonEmptyExtractor[Seq[String]] =
    NonEmptyExtractor(_.getOrElse(key, Nil))

  def OptString(key: String): NonEmptyExtractor[Option[String]] =
    NonEmptyExtractor(_.getOrElse(key, Nil).find(_.nonEmpty))

  val intent: unfiltered.filter.Plan.Intent = {
    import unfiltered.request.{GET, Path, Params, &}

    val ApplyMethods = SeqString(Param.APPLY_METHODS)
    val Reads = OptString(Param.READS)
    val Writes = OptString(Param.WRITES)
    val Format = OptString(Param.FORMAT)
    val PackageName = OptString(Param.PACKAGE_NAME)
    val ObjectName = OptString(Param.OBJECT_NAME)

    val UInt = Function.unlift{ str: String =>
      try {
        Option(str.toInt).filter(n => 1 <= n && n <= 252)
      } catch {
        case _: NumberFormatException =>
          None
      }
    }

    val Numbers: NonEmptyExtractor[Seq[Int]] =
      SeqString(Param.NUMBERS).map(_.collect{UInt})

    val Ranges: NonEmptyExtractor[Seq[IntRange]] =
      SeqString(Param.RANGES).map(_.map(IntRange.fromString).flatten)


    {
      case request @ GET(Path("/23") & Params(
        Param.FINAL.opt(fi) &
        Param.CASE.opt(c) &
        Param.CONSTRUCTOR_PRIVATE.opt(pr) &
        Reads(re) &
        Writes(w) &
        Format(fo) &
        ApplyMethods(a) &
        PackageName(pa) &
        ObjectName(o) &
        Numbers(n) &
        Ranges(ra)
      )) =>

        val param: Param = Param(
          packageName = pa,
          objectName = o,
          numbers = n.toSet,
          ranges = ra.toList,
          applyMethods = a.toList,
          isFinal = fi,
          isCase = c,
          constructorPrivate = pr,
          reads = re,
          writes = w,
          format = fo
        )
        Web.htmlPre(generate(param), Lang.SCALA)

      case request @ GET(Path("/23/sequencer") & Params(PackageName(name))) =>

        Web.htmlPre(
          GenSequencer.gen(name.getOrElse(Generate.defaultPackageName)),
          Lang.SCALA
        )
    }
  }

  def generate(param: Param): String =
    Generate.code(param.toSettings)

  final case class Param(
    packageName: Option[String],
    objectName: Option[String],
    numbers: Set[Int],
    ranges: List[IntRange],
    applyMethods: List[String],
    isFinal: Option[Boolean],
    isCase: Option[Boolean],
    constructorPrivate: Option[Boolean],
    reads: Option[String],
    writes: Option[String],
    format: Option[String]
  ){
    def allRange: Set[Int] = (ranges.flatMap(_.range) ++ numbers).toSet

    def toSettings: Settings = Settings(
      packageName = packageName.getOrElse(Generate.defaultPackageName),
      objectName = objectName.getOrElse(Generate.defaultObjectName),
      params = play.twentythree.Param.params(allRange).mapValues(p =>
        p.copy(
          applyMethods = {
            if(applyMethods.isEmpty) p.applyMethods
            else applyMethods
          },
          clazz = p.clazz.copy(
            isFinal = isFinal.getOrElse(Param.FINAL.default),
            isCase = isCase.getOrElse(Param.CASE.default),
            constructorPrivate = constructorPrivate.getOrElse(Param.CONSTRUCTOR_PRIVATE.default)
          ),
          _reads = reads,
          _writesFast = writes,
          formatFast = {
            if(reads.isEmpty && writes.isEmpty && format.isEmpty) Option("format")
            else format
          }
        )
      )
    )
  }

  object Param {
    final val PACKAGE_NAME = "package_name"
    final val OBJECT_NAME = "object_name"
    final val NUMBERS = "numbers"
    final val RANGES = "ranges"
    final val APPLY_METHODS = "apply_methods"
    final val READS = "reads"
    final val WRITES = "writes"
    final val FORMAT = "format"

    val FINAL = BoolParam("final", true)
    val CASE = BoolParam("case", false)
    val CONSTRUCTOR_PRIVATE = BoolParam("constructor_private", true)

    implicit val instance: CodecJson[Param] =
      CodecJson.casecodec11(apply, unapply)(
        PACKAGE_NAME,
        OBJECT_NAME,
        NUMBERS,
        RANGES,
        APPLY_METHODS,
        FINAL.key,
        CASE.key,
        CONSTRUCTOR_PRIVATE.key,
        READS,
        WRITES,
        FORMAT
      )
  }

  final case class IntRange private(min: Int, max: Int) {
    def encode: String = min + "-" + max
    def range: Seq[Int] = min to max
  }

  object IntRange {
    private[this] object UIntLimit {
      final val max = 252
      def unapply(str: String): Option[Int] = try {
        Option(str.toInt).map{
          case n if n <= 0 => 1
          case n if n > max => max
          case n => n
        }
      } catch {
        case _: NumberFormatException =>
          None
      }
    }

    val fromString: String => Option[IntRange] = str =>
      PartialFunction.condOpt(str.split('-')){
        case Array(UIntLimit(min), UIntLimit(max)) =>
          IntRange(min, max)
    }

    private[this] final val range = "range"
    implicit val instance: CodecJson[IntRange] =
      CodecJson.derived[IntRange](
        EncodeJson.jencode1L[IntRange, String](_.encode)(range),
        DecodeJson(a =>
          DecodeJson.jdecode1L[String, String](identity)(range).decodeJson(a.focus).map(fromString).flatMap{
            case None =>
              DecodeResult.fail("invalid range format", a.history)
            case Some(value) =>
              DecodeResult.ok(value)
          }
        )
      )
  }
}
