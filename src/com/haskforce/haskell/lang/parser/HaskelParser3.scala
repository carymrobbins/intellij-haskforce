package com.haskforce.haskell.lang.parser

import com.haskforce.HaskellLanguage
import com.haskforce.utils.NonEmptySet
import com.intellij.lang.{ASTNode, Language, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType
import io.estatico.generic.traits._
import scalaz.Monad
import scalaz.syntax.monad._
import scalaz.std.list._
import shapeless.{::, Generic, HList, HNil}

class HaskelParser3 extends PsiParser {
  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    ???
  }
}

object HaskellPsiGParsec {

  // Could just be a newtype around Kleisli[Psi.Result, PsiBuilder, A]
  final case class Psi[A](run: PsiBuilder => Psi.Result[A]) {

    def map[B](f: A => B): Psi[B] =
      Psi(b => run(b).map(f))

    def as[B](b: B): Psi[B] = map(_ => b)

    def flatMap[B](f: A => Psi[B]): Psi[B] =
      Psi(b => run(b).flatMap(a => f(a).run(b)))

    def flatMapF[B](f: A => Psi.Result[B]): Psi[B] =
      Psi(b => run(b).flatMap(f))

    def >>=[B](f: A => Psi[B]): Psi[B] = flatMap(f)

    def *>[B](p: Psi[B]): Psi[B] = flatMap(_ => p)

    def <*(p: Psi[_]): Psi[A] = flatMap(a => p.as(a))

    def >>[B](p: => Psi[B]): Psi[B] = flatMap(_ => p)

    def <<(p: => Psi[_]): Psi[A] = flatMap(a => p.as(a))

    def flatten[B](implicit ev: A =:= Psi[B]): Psi[B] =
      Psi(b => run(b).flatMap(a => ev(a).run(b)))

    def orElse(p: Psi[A]): Psi[A] =
      Psi { b =>
        val m = b.mark()
        this.run(b) match {
          case right@Right(_) =>
            m.drop()
            right
          case Left(es1) =>
            m.rollbackTo()
            p.run(b) match {
              case right@Right(_) => right
              case Left(es2) =>
                Left(es1.append(es2))
            }
        }
      }

    def |||(p: Psi[A]): Psi[A] = orElse(p)
  }

  object Psi {

    type Result[A] = Either[NonEmptySet[String], A]

    object Result {
      def success[A](a: A): Result[A] = Right(a)
      val unit: Result[Unit] = Right(())
      def failure[A](message: String): Result[A] = Left(NonEmptySet(message))
    }

    def pure[A](a: A): Psi[A] = Psi(_ => Right(a))

    def failed[A](s: String): Psi[A] = Psi(_ => Left(NonEmptySet(s)))

    def successful[A](f: PsiBuilder => A): Psi[A] =
      new Psi(b => Right(f(b)))

    def tupled[A, B](p1: Psi[A], p2: Psi[B]): Psi[(A, B)] =
      for {
        a <- p1
        b <- p2
      } yield (a, b)

    def whileM[A](b: Psi[Boolean])(pa: Psi[A]): Psi[List[A]] = pa.whileM(b)

    def ifM[A](b: Psi[Boolean])(onTrue: Psi[A])(onFalse: Psi[A]): Psi[A] =
      b.flatMap(if (_) onTrue else onFalse)

    implicit val monad: Monad[Psi] = new Monad[Psi] {
      override def point[A](a: => A): Psi[A] = Psi.pure(a)
      override def bind[A, B](fa: Psi[A])(f: A => Psi[B]): Psi[B] = fa.flatMap(f)
    }
  }

  implicit final class LeftOps[A, B](private val x: Left[A, B]) extends AnyVal {
    def castR[C]: Either[A, C] = x.asInstanceOf[Either[A, C]]
  }

  implicit final class RightOps[A, B](private val x: Right[A, B]) extends AnyVal {
    def castL[C]: Either[C, B] = x.asInstanceOf[Either[C, B]]
  }

  trait IsParsecElement[A] { type Repr <: HList }
  object IsParsecElement {

    type Aux[A, R <: HList] = IsParsecElement[A] { type Repr = R }

    private val cached = new IsParsecElement[Nothing] {}

    def unsafe[A, R <: HList]: IsParsecElement.Aux[A, R] =
      cached.asInstanceOf[IsParsecElement.Aux[A, R]]

    implicit def hlist[H, T <: HList](
      implicit
      ev: IsParsecElement[T]
    ): IsParsecElement.Aux[H :: T, Parsed[H] :: ev.Repr] = unsafe

    implicit val hnil: IsParsecElement.Aux[HNil, HNil] = unsafe

    implicit def derived[A, R <: HList](
      implicit
      g: Generic.Aux[A, R],
      ev: IsParsecElement[R]
    ): IsParsecElement.Aux[A, ev.Repr] = unsafe
  }

  abstract class ParsecNode[A : IsParsecElement](
    name: String,
    language: Language
  ) extends IElementType(name, language)

  sealed trait Parsed[A]
  object Parsed {

    private val cached = new Parsed[Nothing] {}

    def unsafe[A]: Parsed[A] = cached.asInstanceOf[Parsed[A]]

    def sequence[F[_], A](fa: F[Parsed[A]]): Parsed[F[A]] = unsafe

    implicit final class FParsedOps[F[_], A](
      private val self: F[Parsed[A]]
    ) extends AnyVal {
      def sequence: Parsed[F[A]] = unsafe
    }
  }

  trait Parsec {

    type Token <: IElementType
    type Node[A] <: ParsecNode[A]

    def getTokenType: Psi[Option[IElementType]] =
      Psi.successful(b => Option(b.getTokenType))

    def lookAhead(n: Int): Psi[Option[IElementType]] =
      Psi.successful(b => Option(b.lookAhead(n)))

    /** Run parser without consuming input, checking if it succeeds or not. */
    def peek(p: Psi[_]): Psi[Boolean] =
      Psi.successful { b =>
        val m = b.mark()
        val res = p.run(b)
        m.rollbackTo()
        res.isRight
      }

    def eof: Psi[Boolean] =
      Psi.successful(b => b.eof())

    def accept(t: Token): Psi[Unit] =
      Psi(b =>
        if (b.getTokenType == t) {
          b.advanceLexer()
          Psi.Result.unit
        } else {
          Psi.Result.failure(s"Expected $t")
        }
      )

    def remap[A](t: Token, n: Node[A]): Psi[Parsed[A]] =
      Psi(b =>
        if (b.getTokenType == t) {
          b.remapCurrentToken(n)
          b.advanceLexer()
          Psi.Result.success(Parsed.unsafe)
        } else {
          Psi.Result.failure(s"Expected $t")
        }
      )

    def optional[A](p: Psi[A]): Psi[Option[A]] =
      Psi.successful { b =>
        val m = b.mark()
        p.run(b) match {
          case Left(_) =>
            m.rollbackTo()
            None
          case Right(a) =>
            m.drop()
            Some(a)
        }
      }

    def many[A](p: Psi[A]): Psi[List[A]] =
      Psi.whileM(peek(p))(p)

    def node[A, H <: HList](n: Node[A])(body: Psi[H])(
      implicit ev: IsParsecElement.Aux[A, H]
    ): Psi[Parsed[A]] = Psi { b =>
      val m = b.mark()
      body.run(b) match {
        case Right(_) =>
          m.done(n)
          unsafeParsedSuccess[A]
        case left@Left(es) =>
          m.error(mkError(es))
          left.castR
      }
    }

    private def mkError(es: NonEmptySet[String]): String =
      es.iterator.mkString("\n")

    private def unsafeParsedSuccess[A]: Psi.Result[Parsed[A]] =
      unsafeParsedSuccessCached.asInstanceOf[Psi.Result[Parsed[A]]]

    private val unsafeParsedSuccessCached = Parsed.unsafe
  }

  //noinspection TypeAnnotation
  object HaskellTokens {

    final case class Token(name: String)
      extends IElementType(name, HaskellLanguage.INSTANCE)

    val CONID = Token("CONID")
    val DOT = Token("DOT")
  }

  //noinspection TypeAnnotation
  object HaskellNodes {

    final case class Node[A : IsParsecElement](name: String)
      extends ParsecNode(name, HaskellLanguage.INSTANCE)

    import HaskellElements._

    val CONID = Node[Conid]("CONID")
    val NAMESPACE = Node[Namespace]("NAMESPACE")
    val QCONID = Node[QConid]("QCONID")
  }

  //noinspection TypeAnnotation
  object HaskellElements {

    trait Conid
    object Conid {
      implicit val generic: Generic.Aux[Conid, HNil] =
        unsafeDeriveEmptyGeneric(() => new Conid {})
    }

    trait Namespace {
      def conids: List[Conid]
    }

    trait QConid {
      def namespace: Option[Namespace]
      def conid: Conid
    }
  }

  // TODO: generic-traits should support empty traits.
  private def unsafeDeriveEmptyGeneric[A](f: () => A): Generic.Aux[A, HNil] =
    new Generic[A] {
      override type Repr = HNil
      override def to(t: A): HNil = HNil
      override def from(r: HNil): A = f()
    }

  //noinspection TypeAnnotation
  object HaskellParsec extends Parsec {

    val T = HaskellTokens
    val N = HaskellNodes
    val E = HaskellElements

    override type Token = T.Token
    override type Node[A] = N.Node[A]

    val pConid: Psi[Parsed[E.Conid]] = remap(T.CONID, N.CONID)

    val pDot: Psi[Unit] = accept(T.DOT)

    private val pConidDot = pConid <* pDot

    // TODO: We need to ensure there are no space tokens between conid and dot here.
    // Can maybe use rawLookup for this?
    val pNamespace: Psi[Parsed[E.Namespace]] =
      node(N.NAMESPACE)(
        Psi.whileM(peek(pConidDot))(pConidDot).flatMapF {
          case Nil => Psi.Result.failure("Expected Namespace")
          case xs  => Psi.Result.success(xs.sequence :: HNil)
        }
      )

    val pQConid: Psi[Parsed[E.QConid]] =
      node(N.QCONID)(for {
        ns <- optional(pNamespace)
        cid <- pConid
      } yield ns.sequence :: cid :: HNil)
  }
}
