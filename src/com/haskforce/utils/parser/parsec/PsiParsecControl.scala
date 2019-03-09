package com.haskforce.utils.parser.parsec

object PsiParsecControl {

  /** Monadic if combinator for [[Psi]]. */
  final case class pif(if0: Psi[Boolean]) {

    /** Syntax sugar for building an if-like expression with explicit
      * pthen/pelse branches.
      */
    case class pthen[A](then0: Psi[A]) {
      def pelse(else0: Psi[A]): Psi[A] = pif.this.apply[A](then0)(else0)
    }

    /** Syntax sugar-free if-like expression. */
    def apply[A](then0: Psi[A])(else0: Psi[A]): Psi[A] =
      if0.flatMap(if (_) then0 else else0)
  }
}
