package com.haskforce.utils.parser.parsec

import com.intellij.psi.tree.IElementType
import scalaz.syntax.monad._

object PsiParsecUntypedElements
  extends PsiParsecTypedElements[IElementType, IElementType]

/**
  * It is the responsibility of the implementer of the lexer to ensure that all
  * tokens produced conform to the type of 'I'. This can be enforced by
  * specifying the appropriate token type via the '%type' directive in a
  * JFlex lexer.
  */
abstract class PsiParsecTypedElements[I <: IElementType, O <: IElementType]
    extends PsiParsecCore {

  def markStart[A](p: Psi[(MarkResult[O], A)]): Psi[A] = for {
    marker <- Psi(_.mark())
    res0 <- p
    (markRes, parseRes) = res0
    _ <- pure(markRes match {
      case MarkResult.Done(el) => marker.done(el)
      case MarkResult.Collapse(el) => marker.collapse(el)
      case MarkResult.Error(msg) => marker.error(msg)
    })
  } yield parseRes

  def markDone(el: O): Psi[(MarkResult[O], Unit)] =
    pure((MarkResult.Done(el), ()))

  def markDoneWith[A](el: O, a: A): Psi[(MarkResult[O], A)] =
    pure((MarkResult.Done(el), a))

  def markError(msg: String): Psi[(MarkResult[O], Unit)] =
    pure((MarkResult.Error(msg), ()))

  def lookAhead(n: Int): Psi[Option[I]] =
    Psi(b => castTokenF(Option(b.lookAhead(n))))

  def lookAheadMany(n: Int): Psi[List[I]] = {
    if (n < 0) throw new IllegalArgumentException("Cannot take a negative number of tokens!")
    Psi(b => castTokenF((0 until n).map(b.lookAhead).takeWhile(_ != null).toList))
  }

  def remapAdvance(el: O): Psi[Unit] =
    Psi(_.remapCurrentToken(el)) *> advanceLexer

  val getTokenType: Psi[Option[I]] =
    Psi(b => castTokenF(Option(b.getTokenType)))

  def withTokenType(f: I => Psi[Unit]): Psi[Unit] =
    getTokenType >>= {
      case Some(el) => f(el)
      case None => error("Unexpected end of input")
    }

  def expectTokenAdvance(el: I): Psi[Unit] =
    withTokenType(el0 => if (el0 == el) advanceLexer else error(s"Expected $el"))

  def expectTokenOneOfAdvance(els: I*): Psi[Unit] =
    withTokenType(el0 =>
      if (els.contains(el0)) advanceLexer
      else error(s"Expected one of " + els.mkString(", "))
    )

  def maybeTokenAdvance(el: I): Psi[Boolean] =
    getTokenType.flatMap {
      case Some(el0) if el0 == el => advanceLexer *> rTrue
      case _ => rFalse
    }

  def maybeTokenOneOfAdvance(els: I*): Psi[Boolean] =
    getTokenType.flatMap {
      case Some(el0) if els.contains(el0) => advanceLexer *> rTrue
      case _ => rFalse
    }

  def whenTokenIs(p: I => Boolean)(b: Psi[Unit]): Psi[Boolean] =
    getTokenType >>= {
      case Some(t) if p(t) => b *> rTrue
      case _ => rFalse
    }

  def whenTokenIs_(p: I => Boolean)(b: Psi[Unit]): Psi[Unit] =
    getTokenType >>= {
      case Some(t) if p(t) => b
      case _ => rUnit
    }

  def whenTokenIs(el: I)(p: Psi[Unit]): Psi[Boolean] =
    getTokenType >>= { ot =>
      if (ot.contains(el)) p *> rTrue else rFalse
    }

  def whenTokenIs_(el: I)(p: Psi[Unit]): Psi[Unit] =
    getTokenType >>= { ot =>
      if (ot.contains(el)) p else rUnit
    }

  def whenTokenIsAdvance(el: I)(p: Psi[Unit]): Psi[Unit] =
    withTokenType(el0 => if (el0 == el) advanceLexer *> p else rUnit)

  def many_(els: I*): Psi[Unit] =
    Psi(b => while (els.contains(b.getTokenType)) b.advanceLexer())

  def manyp_(ps: Psi[Boolean]*): Psi[Unit] =
    Psi(b => while (ps.exists(_.run(b))) b.advanceLexer())

  private def castToken(x: IElementType): I = x.asInstanceOf[I]

  private def castTokenF[F[_]](x: F[IElementType]): F[I] = x.asInstanceOf[F[I]]
}
