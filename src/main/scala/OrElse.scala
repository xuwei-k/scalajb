package com.github.xuwei_k.scalajb

import argonaut.{EncodeJson, DecodeJson}
import scalaz.Isomorphism.{<~~>, IsoBifunctorTemplate}
import scalaz._

final case class OrElse[A, B](run: A \/ B) extends AnyVal {
  def ===(that: A OrElse B)(implicit A: Equal[A], B: Equal[B]): Boolean =
    this.run === that.run
}

object OrElse {

  def right[A, B](b: B): A OrElse B =
    OrElse(\/-(b))

  def r[A, B]: B => (A OrElse B) =
    right[A, B]

  def left[A, B](a: A): A OrElse B =
    OrElse(-\/(a))

  def l[A, B]: A => (A OrElse B) =
    left[A, B]

  implicit def orElseDecodeJson[A, B](implicit A: DecodeJson[A], B: DecodeJson[B]): DecodeJson[A OrElse B] =
    DecodeJson[A OrElse B](c => {
      val q = A.apply(c).map(OrElse.l[A, B])
      q.result.fold(_ => B(c).map(OrElse.r), _ => q)
    })

  implicit def orElseEncodeJson[A, B](implicit A: EncodeJson[A], B: EncodeJson[B]): EncodeJson[A OrElse B] =
    EncodeJson[A OrElse B](_.run.fold(A.apply, B.apply))

  implicit val orElseDisjunctionIso: OrElse <~~> \/ =
    new IsoBifunctorTemplate[OrElse, \/] {
      override def from[A, B](fa: \/[A, B]) =
        OrElse(fa)
      override def to[A, B](ga: OrElse[A, B]) =
        ga.run
    }

  implicit val orElseInstance2: Bitraverse[OrElse] =
    new IsomorphismBitraverse[OrElse, \/] {
      def G = \/.DisjunctionInstances2
      def iso = orElseDisjunctionIso
    }

  implicit def orElseInstances1[L]:
    Traverse[({type l[a] = L OrElse a})#l] with
    Monad[({type l[a] = L OrElse a})#l] with
    Optional[({type l[a] = L OrElse a})#l] with
    Plus[({type l[a] = L OrElse a})#l] = new
      IsomorphismTraverse[({type l[a] = L OrElse a})#l, ({type l[a] = L \/ a})#l] with
      IsomorphismMonad[({type l[a] = L OrElse a})#l, ({type l[a] = L \/ a})#l] with
      IsomorphismPlus[({type l[a] = L OrElse a})#l, ({type l[a] = L \/ a})#l] with
      IsomorphismOptional[({type l[a] = L OrElse a})#l, ({type l[a] = L \/ a})#l] {
        def G = \/.DisjunctionInstances1
        def iso = orElseDisjunctionIso.unlift1[L]
    }

  implicit def orElseEqual[A: Equal, B: Equal]: Equal[A OrElse B] =
    Equal.equalBy(_.run)

  implicit def toInvariantFunctorOps[A, B](a: A OrElse B) =
    scalaz.syntax.invariantFunctor.ToInvariantFunctorOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toFunctorOps[A, B](a: A OrElse B) =
    scalaz.syntax.functor.ToFunctorOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toApplyOps[A, B](a: A OrElse B) =
    scalaz.syntax.apply.ToApplyOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toApplicativeOps[A, B](a: A OrElse B) =
    scalaz.syntax.applicative.ToApplicativeOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toBindOps[A, B](a: A OrElse B) =
    scalaz.syntax.bind.ToBindOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toMonadOps[A, B](a: A OrElse B) =
    scalaz.syntax.monad.ToMonadOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toPlusOps[A, B](a: A OrElse B) =
    scalaz.syntax.plus.ToPlusOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toFoldableOps[A, B](a: A OrElse B) =
    scalaz.syntax.foldable.ToFoldableOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toTraverseOps[A, B](a: A OrElse B) =
    scalaz.syntax.traverse.ToTraverseOps[({type l[a] = A OrElse a})#l, B](a)

  implicit def toOptionalOps[A, B](a: A OrElse B) =
    scalaz.syntax.optional.ToOptionalOps[({type l[a] = A OrElse a})#l, B](a)

}

