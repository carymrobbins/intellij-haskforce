package com.haskforce.utils

/** Functional combinators for Java interop. */
object FunctionUtil {
  def identity[A]: A => A = scala.Predef.identity
}
