package com.haskforce.utils.parser

import com.intellij.lang.{ASTNode, PsiBuilder}
import com.intellij.psi.tree.IElementType
import scalaz.Reader
import scalaz.syntax.monad._

import scala.language.higherKinds

object PsiParsec {
  type Psi[A] = Reader[PsiBuilder, A]
  def Psi[A](f: PsiBuilder => A): Psi[A] = Reader(f)

  sealed trait MarkResult
  object MarkResult {
    final case class Done(elementType: IElementType) extends MarkResult
    final case class Collapse(elementType: IElementType) extends MarkResult
    final case class Error(message: String) extends MarkResult
  }

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

  def markStart[A](p: Psi[(MarkResult, A)]): Psi[A] = for {
    marker <- Psi(_.mark())
    res0 <- p
    (markRes, parseRes) = res0
    _ <- pure(markRes match {
      case MarkResult.Done(el) => marker.done(el)
      case MarkResult.Collapse(el) => marker.collapse(el)
      case MarkResult.Error(msg) => marker.error(msg)
    })
  } yield parseRes

  def markDone(el: IElementType): Psi[(MarkResult, Unit)] =
    pure((MarkResult.Done(el), ()))

  def markError(msg: String): Psi[(MarkResult, Unit)] =
    pure((MarkResult.Error(msg), ()))

  def lookAhead(n: Int): Psi[IElementType] = Psi(_.lookAhead(n))

  val getTreeBuilt: Psi[ASTNode] = Psi(_.getTreeBuilt)

  val eof: Psi[Boolean] = Psi(_.eof())

  val advanceLexer: Psi[Unit] = Psi(_.advanceLexer())

  def remapAdvance(el: IElementType): Psi[Unit] =
    Psi(_.remapCurrentToken(el)) *> advanceLexer

  val getTokenType: Psi[Option[IElementType]] =
    Psi(b => Option(b.getTokenType))

  def error(msg: String): Psi[Unit] = Psi(_.error(msg))

  def withTokenType(f: IElementType => Psi[Unit]): Psi[Unit] =
    getTokenType >>= {
      case Some(el) => f(el)
      case None => error("Unexpected end of input")
    }

  def expectTokenAdvance(el: IElementType): Psi[Unit] =
    withTokenType(el0 => if (el0 == el) advanceLexer else error(s"Expected $el"))

  def expectTokenOneOfAdvance(els: IElementType*): Psi[Unit] =
    withTokenType(el0 =>
      if (els.contains(el0)) advanceLexer
      else error(s"Expected one of " + els.mkString(", "))
    )

  def expectAdvance(p: Psi[Boolean], msg: String): Psi[Unit] =
    p.flatMap(b => error(msg).whenM(!b))

  def maybeTokenAdvance(el: IElementType): Psi[Boolean] =
    getTokenType.flatMap {
      case Some(el0) if el0 == el => advanceLexer *> rTrue
      case _ => rFalse
    }

  def maybeTokenOneOfAdvance(els: IElementType*): Psi[Boolean] =
    getTokenType.flatMap {
      case Some(el0) if els.contains(el0) => advanceLexer *> rTrue
      case _ => rFalse
    }

  def maybeAdvance(p: Psi[Boolean]): Psi[Boolean] =
    p.flatMap(b => advanceLexer.whenM(b) *> pure(b))

  def advanceWhile(p: Psi[Boolean])(b: Psi[Unit]): Psi[Unit] =
    (b *> advanceLexer).whileM_(for {
      isEof <- eof
      x <- p
    } yield !isEof && x)

  def whenTokenIs(p: IElementType => Boolean)(b: Psi[Unit]): Psi[Unit] =
    withTokenType(el0 => b.whenM(p(el0)))

  val consumeUntilEOF: Psi[Unit] = advanceWhile(rTrue)(rUnit)
}
