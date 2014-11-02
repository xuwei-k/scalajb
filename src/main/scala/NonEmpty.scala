package com.github.xuwei_k.scalajb

final case class NonEmpty[A](get: A){
  def isEmpty = false

  def map[B](f: A => B): NonEmpty[B] =
    NonEmpty(f(get))
}
