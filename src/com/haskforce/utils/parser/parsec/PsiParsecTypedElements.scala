package com.haskforce.utils.parser.parsec

import com.intellij.psi.tree.IElementType
import scalaz.syntax.monad._

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

  def markError(msg: String): Psi[(MarkResult[O], Unit)] =
    pure((MarkResult.Error(msg), ()))

  def lookAhead(n: Int): Psi[Option[I]] =
    Psi(b => castTokenF(Option(b.lookAhead(n))))

  def remapAdvance(el: I): Psi[Unit] =
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

  def whenTokenIs(p: I => Boolean)(b: Psi[Unit]): Psi[Unit] =
    withTokenType(el0 => b.whenM(p(el0)))

  private def castToken(x: IElementType): I = x.asInstanceOf[I]

  private def castTokenF[F[_]](x: F[IElementType]): F[I] = x.asInstanceOf[F[I]]
}
