package com.haskforce.system.utils

import org.jetbrains.annotations.Nullable

object NullUtil {

  /**
   * Analogous to Option(..).map(..).getOrElse(..) except deals with a Nullable value
   * and does not wrap the passed value in an Option.
   */
  def fold[A, X](@Nullable a: A)(ifNull: => X, ifNotNull: A => X): X = {
    if (a == null) ifNull else ifNotNull(a)
  }
}
