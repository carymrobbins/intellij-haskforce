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

  def rightMap[A, B, C](e: Either[A, B], f: B => C): Either[A, C] = {
    e.right.map(f)
  }

  def rightFlatMap[A, B, C](e: Either[A, B], f: B => Either[A, C]): Either[A, C] = {
    e.right.flatMap(f)
  }

  def left[A, B](x: A): Either[A, B] = Left(x)

  def right[A, B](x: B): Either[A, B] = Right(x)
}
