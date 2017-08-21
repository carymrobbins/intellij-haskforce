package com.haskforce.utils

import prelude._

import com.intellij.openapi.application.Application
import com.intellij.openapi.util.Computable

/** Provides safe monadic access to IntelliJ's read model. */
sealed trait IJReadAction[+A] {

  def unsafeRunReadAction(): A

  final def run(app: Application): A = {
    app.runReadAction(new Computable[A] {
      override def compute(): A = unsafeRunReadAction()
    })
  }

  final def map[B](f: A => B): IJReadAction[B] = IJReadAction(f(unsafeRunReadAction()))

  final def flatMap[B](f: A => IJReadAction[B]): IJReadAction[B]
    = IJReadAction(f(unsafeRunReadAction()).unsafeRunReadAction())
}

object IJReadAction {

  def apply[A](f: => A): IJReadAction[A] = new IJReadAction[A] {
    override def unsafeRunReadAction(): A = f
  }

  implicit val monad: Monad[IJReadAction] = new Monad[IJReadAction] {

    override def point[A](a: => A): IJReadAction[A] = IJReadAction(a)

    override def bind[A, B](fa: IJReadAction[A])(f: A => IJReadAction[B]): IJReadAction[B]
      = fa.flatMap(f)

    /**
     * Performs better O(1) than the default traverse O(n)
     * The default traverse will create a new [[IJReadAction]] instance for each
     * element in value, whereas this implementation will only create one new instance.
     */
    override def traverse[A, G[_], B](value: G[A])(f: A => IJReadAction[B])(
      implicit G: Traverse[G]
    ): IJReadAction[G[B]] = IJReadAction(G.map(value)(a => f(a).unsafeRunReadAction()))
  }
}
