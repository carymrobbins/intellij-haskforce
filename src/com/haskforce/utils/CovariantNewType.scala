package com.haskforce.utils

import io.estatico.newtype.{BaseNewType, Coercible}

/** NewType to carry subtype information in Type[A].  */
trait CovariantNewType extends BaseCovariantNewType {
  type Top = Type[SuperType]
  type Base[A] = { type Repr = A }
}

object CovariantNewType {
  trait Of[S] extends BaseCovariantNewType.Of[S] with CovariantNewType
  trait Default[S] extends Of[S] with BaseCovariantNewTypeExtras.All
}

trait BaseCovariantNewType {
  type SuperType
  type Base[A]
  trait Tag[A]
  final type Type[A] = BaseNewType.Aux[Base[A], Tag[A], A]

  // Define Coercible instances for which we can safely cast to/from.
  @inline implicit def wrap[A <: SuperType]: Coercible[A, Type[A]] = Coercible.instance
  @inline implicit def unwrap[A <: SuperType]: Coercible[Type[A], A] = Coercible.instance
  @inline implicit def wrapM[M[_], A <: SuperType]: Coercible[M[A], M[Type[A]]] = Coercible.instance
  @inline implicit def unwrapM[M[_], A <: SuperType]: Coercible[M[Type[A]], M[A]] = Coercible.instance
  @inline implicit def convert[N <: BaseNewType.Aux[_, _, A], A <: SuperType]: Coercible[Type[A], N] = Coercible.instance
}

object BaseCovariantNewType {

  trait Of[S] extends BaseCovariantNewType {
    final type SuperType = S
  }
}

object BaseCovariantNewTypeExtras {

  trait All extends ApplyM

  trait Apply extends BaseCovariantNewType {
    @inline final def apply[A <: SuperType](a: A): Type[A] = a.asInstanceOf[Type[A]]
  }

  trait ApplyM extends Apply {
    @inline final def applyM[M[_], A <: SuperType](a: M[A]): M[Type[A]] = a.asInstanceOf[M[Type[A]]]
  }
}

