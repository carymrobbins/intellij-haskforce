package com.haskforce.utils

/** Simplify Either interop with Java. */
object EitherUtil {

  def unsafeGetLeft[A, B](e: Either[A, B]): A = {
    e.fold(
      left => left,
      right => throw new RuntimeException(s"Value was right: $right")
    )
  }

  def unsafeGetRight[A, B](e: Either[A, B]): B = {
    e.fold(
      left => throw new RuntimeException(s"Value was left: $left"),
      right => right
    )
  }

  def left[A, B](x: A): Either[A, B] = Left(x)

  def right[A, B](x: B): Either[A, B] = Right(x)

  def valueOr[A, B](e: Either[A, B], f: A => B): B = e match {
    case Left(a) => f(a)
    case Right(b) => b
  }
}
