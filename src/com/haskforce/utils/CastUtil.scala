package com.haskforce.utils

import scala.reflect.ClassTag

/** Utilities for safely casting values. */
object CastUtil {

  /** Cast a value from Any type to the specified type. */
  def fromAny[A : ClassTag](x: Any): Option[A] = {
    implicitly[ClassTag[A]].unapply(x)
  }

  /**
   * Safely cast a value to a subtype, rejecting cases, at compile time,
   * when A is not a subtype of the value's type.
   */
  def down[A]: _Down[A] = _down.asInstanceOf[_Down[A]]

  /** Utility class used to curry the type parameter for the 'down' method. */
  final class _Down[A] private[CastUtil] {
    def apply[B](b: B)(implicit ct: ClassTag[A], ev: A <:< B): Option[A] = {
      ct.unapply(b)
    }
  }
  private lazy val _down = new _Down[Nothing]
}
