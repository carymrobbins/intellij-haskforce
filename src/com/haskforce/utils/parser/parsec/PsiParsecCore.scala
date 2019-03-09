package com.haskforce.utils.parser.parsec

import com.intellij.lang.{ASTNode, PsiBuilder}
import scalaz.syntax.monad._

import scala.language.higherKinds

trait PsiParsecCore {

  def pure[A](f: => A): Psi[A] = Psi(_ => f)

  val rUnit: Psi[Unit] = pure(())
  val rFalse: Psi[Boolean] = pure(false)
  val rTrue: Psi[Boolean] = pure(true)
  def rNone[A]: Psi[Option[A]] = _rNone.asInstanceOf[Psi[Option[A]]]

  // Cached value for rNone.
  private val _rNone: Psi[None.type] = pure(None)

  /**
    * Useful for inspecting a combinator, particularly for inserting a
    * breakpoint for debugging.
    */
  def inspect[A](f: PsiBuilder => Psi[A]): Psi[A] =
    Psi[Psi[A]](b => f(b)).join[A]

  def TODO[A]: Psi[A] = throw new RuntimeException("PsiParsec TODO")

  val pif: PsiParsecControl.pif.type = PsiParsecControl.pif

  def pwhen(t: Boolean)(x: Psi[Unit]): Psi[Boolean] =
    if (t) x *> rTrue else rFalse

  def pwhen_(t: Boolean)(x: Psi[Unit]): Psi[Unit] =
    if (t) x else rUnit

  def punless(t: Boolean)(x: Psi[Unit]): Psi[Boolean] =
    if (!t) x *> rTrue else rFalse

  def punless_(t: Boolean)(x: Psi[Unit]): Psi[Unit] =
    if (!t) x else rUnit

  def pwhenM(t: Psi[Boolean])(x: Psi[Unit]): Psi[Boolean] =
    t.flatMap(pwhen(_)(x))

  def pwhenM_(t: Psi[Boolean])(x: Psi[Unit]): Psi[Unit] =
    t.flatMap(pwhen_(_)(x))

  def punlessM(t: Psi[Boolean])(x: Psi[Unit]): Psi[Boolean] =
    t.flatMap(punless(_)(x))

  def punlessM_(t: Psi[Boolean])(x: Psi[Unit]): Psi[Unit] =
    t.flatMap(punless_(_)(x))

  def pseq_(ps: Psi[Unit]*): Psi[Unit] = {
    // Use a mutable var for efficiency.
    var res = rUnit
    ps.foreach(p => res = res *> p)
    res
  }

  val getTreeBuilt: Psi[ASTNode] = Psi(_.getTreeBuilt)

  val eof: Psi[Boolean] = Psi(_.eof())

  def not(p: Psi[Boolean]): Psi[Boolean] = p.map(!_)

  val advanceLexer: Psi[Unit] = Psi(_.advanceLexer())

  def times(p: Psi[Unit], n: Int): Psi[Unit] = {
    if (n < 0) throw new IllegalArgumentException("Cannot run parser negative times!")
    Psi { b =>
      var i = 0
      while (i != n) {
        p.run(b)
        i += 1
      }
    }
  }

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

  /** Run parser until eof is reached. */
  def pforever(p: Psi[Unit]): Psi[Unit] =
    Psi(b => while (!b.eof()) p.run(b))

  def pany(ps: Psi[Boolean]*): Psi[Boolean] =
    Psi(b => ps.exists(_.run(b)))

  def pany_(ps: Psi[Boolean]*): Psi[Unit] =
    pany(ps: _*).void
}
