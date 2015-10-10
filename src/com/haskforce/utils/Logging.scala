package com.haskforce.utils

import com.intellij.openapi.diagnostic.Logger

/**
 * Mixin providing logging facilities.
 */
trait Logging {
  lazy val LOG: Logger = Logger.getInstance(getClass)

  def traceLn[A](prefix: String, a: A): A = {
    println(s"$prefix: $a")
    a
  }
}
