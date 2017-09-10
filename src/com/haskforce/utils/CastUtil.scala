package com.haskforce.utils

import scala.reflect.ClassTag

/** Utility for safely casting values. */
object CastUtil {

  def down[A]: _Down[A] = _down.asInstanceOf[_Down[A]]

  final class _Down[A] private[CastUtil] {
    def apply[B](b: B)(implicit ct: ClassTag[A], ev: A <:< B): Option[A] = {
      ct.unapply(b)
    }
  }
  private lazy val _down = new _Down[Nothing]

  implicit final class Ops[A](val repr: A) extends AnyVal {
    def cast[B](implicit ct: ClassTag[B], ev: B <:< A): Option[B] = ct.unapply(repr)
  }
}
