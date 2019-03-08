package com.haskforce.utils.parser.parsec

import com.intellij.lang.ASTNode
import scalaz.syntax.monad._

import scala.language.higherKinds

trait PsiParsecCore {

  def pure[A](f: => A): Psi[A] = Psi(_ => f)

  val rUnit: Psi[Unit] = pure(())
  val rFalse: Psi[Boolean] = pure(false)
  val rTrue: Psi[Boolean] = pure(true)

  def pwhen(t: Boolean)(x: Psi[Unit]): Psi[Unit] =
    if (t) x else rUnit

  def pwhenM(t: Psi[Boolean])(x: Psi[Unit]): Psi[Unit] =
    t.flatMap(pwhen(_)(x))

  def pseq_(ps: Psi[Unit]*): Psi[Unit] = {
    // Use a mutable var for efficiency.
    var res = rUnit
    ps.foreach(p => res = res *> p)
    res
  }

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

  val getTreeBuilt: Psi[ASTNode] = Psi(_.getTreeBuilt)

  val eof: Psi[Boolean] = Psi(_.eof())

  val advanceLexer: Psi[Unit] = Psi(_.advanceLexer())

  def error(msg: String): Psi[Unit] = Psi(_.error(msg))

  def expectAdvance(p: Psi[Boolean], msg: String): Psi[Unit] =
    p.flatMap(b => error(msg).whenM(!b))

  def maybeAdvance(p: Psi[Boolean]): Psi[Boolean] =
    p.flatMap(b => advanceLexer.whenM(b) *> pure(b))

  def advanceWhile(p: Psi[Boolean])(b: Psi[Unit]): Psi[Unit] =
    (b *> advanceLexer).whileM_(for {
      isEof <- eof
      x <- p
    } yield !isEof && x)

  val consumeUntilEOF: Psi[Unit] = advanceWhile(rTrue)(rUnit)
}
