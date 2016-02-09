package com.haskforce.highlighting.annotation.external

/** Wraps a future which can define its own terms for handling failure. */
trait WrappedFuture[A] {
  def get: A
}
