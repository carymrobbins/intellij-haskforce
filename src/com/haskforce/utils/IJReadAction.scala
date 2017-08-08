package com.haskforce.utils

import prelude._

import com.intellij.execution.actions.ConfigurationContext
import com.intellij.openapi.application.Application
import com.intellij.openapi.util.Computable

/** Provides safe monadic access to IntelliJ's read model. */
sealed trait IJReadAction[+A] {

  def unsafeRunReadAction(): A

  final def run[R](r: R)(implicit runner: IJReadActionRunner[R]): A = runner.run(r, this)

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

trait IJReadActionRunner[R] {
  def run[A](r: R, a: IJReadAction[A]): A
}

object IJReadActionRunner {

  /** Read contexts can be safely run with an Application instance. */
  implicit val app: IJReadActionRunner[Application] = new IJReadActionRunner[Application] {
    override def run[A](r: Application, a: IJReadAction[A]): A = r.runReadAction(new Computable[A] {
      override def compute(): A = a.unsafeRunReadAction()
    })
  }

  /** When given a ConfigurationContext, we should already be in a read context. */
  implicit val configContext: IJReadActionRunner[ConfigurationContext] = new IJReadActionRunner[ConfigurationContext] {
    override def run[A](a: ConfigurationContext, r: IJReadAction[A]): A = r.unsafeRunReadAction()
  }
}
