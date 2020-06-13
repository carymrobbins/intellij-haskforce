package prelude.ops

import scalaz.{NonEmptyList, Semigroup, Validation, ValidationNel}

import scala.language.implicitConversions
import scala.util.control.NonFatal

final class EitherObjOps(private val self: Either.type) extends AnyVal {

  def catchNonFatal[A](x: => A): Either[Throwable, A] =
    try {
      Right(x)
    } catch {
      case NonFatal(e) => Left(e)
    }
}

final class EitherOps[A, B](private val self: Either[A, B]) extends AnyVal {

  def valueOr(f: A => B): B = self match {
    case Left(a) => f(a)
    case Right(b) => b
  }

  def orElse(x: Either[A, B]): Either[A, B] = self match {
    case Left(_) => x
    case Right(_) => self
  }

  /** Same as `orElse` except accumulates lefts via Semigroup. */
  def orElseAccum(x: Either[A, B])(implicit A: Semigroup[A]): Either[A, B] =
    self match {
      case Right(_) => self
      case Left(a1) =>
        x match {
          case Right(_) => x
          case Left(a2) => Left(A.append(a1, a2))
        }
    }

  def leftMap[C](f: A => C): Either[C, B] = self match {
    case Left(a) => Left(f(a))
    case x@Right(_) => new RightOps(x).castL
  }

  def leftMapNel: Either[NonEmptyList[A], B] = leftMap(NonEmptyList(_))

  def validationNel: ValidationNel[A, B] = self match {
    case Left(a) => Validation.failure(NonEmptyList(a))
    case Right(b) => Validation.success(b)
  }
}

final class LeftOps[A, B](private val self: Left[A, B]) extends AnyVal {
  def castR[C]: Left[A, C] = self.asInstanceOf[Left[A, C]]
}

final class RightOps[A, B](private val self: Right[A, B]) extends AnyVal {
  def castL[C]: Right[C, B] = self.asInstanceOf[Right[C, B]]
}

trait ToEitherOps {
  implicit def toPreludeEitherObjOps(x: Either.type): EitherObjOps = new EitherObjOps(x)
  implicit def toPreludeEitherOps[A, B](x: Either[A, B]): EitherOps[A, B] = new EitherOps(x)
  implicit def toPreludeLeftOps[A, B](x: Left[A, B]): LeftOps[A, B] = new LeftOps(x)
  implicit def toPreludeRightOps[A, B](x: Right[A, B]): RightOps[A, B] = new RightOps(x)
}
