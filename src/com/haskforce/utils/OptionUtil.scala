package com.haskforce.utils

import java.util

/** Simplify Option interop with Java. */
object OptionUtil {

  def fold[A, B]
      (o: Option[A],
       ifNone: util.function.Function[Void, B],
       ifSome: util.function.Function[A, B])
      : B = o match {
    case None => ifNone(null)
    case Some(x) => ifSome(x)
  }
}
