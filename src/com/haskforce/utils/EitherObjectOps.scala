package com.haskforce.utils

final class EitherObjectOps(private val eitherObj: Either.type) extends AnyVal {

  def nonNull[L, R](r: R, l: L): Either[L, R] =
    if (r == null) Left(l) else Right(r)
}

trait ToEitherObjectOps {
  implicit def toEitherObjectOps(x: Either.type): EitherObjectOps = new EitherObjectOps(x)
}

object ToEitherObjectOps extends ToEitherObjectOps
