package com.haskforce.utils

import java.util.concurrent.ConcurrentHashMap

/**
  * Scala wrapper around ConcurrentHashMap which improves type safety
  * by enforcing the type parameters in method arguments as opposed to
  * AnyRef/Object.
  */
final class SConcurrentHashMap[K, V](
  val toJava: ConcurrentHashMap[K, V]
) extends AnyVal {

  def get(k: K): Option[V] = Option(toJava.get(k))

  def getOrElse(k: K, default: => V): V = {
    val v = toJava.get(k)
    if (v == null) default else v
  }

  def put(k: K, v: V): Unit = toJava.put(k, v)

  def contains(k: K): Boolean = toJava.contains(k)

  def foreach(f: (K, V) => Unit): Unit = toJava.forEach((k, v) => f(k, v))
}

object SConcurrentHashMap {

  def create[K, V](): SConcurrentHashMap[K, V] =
    new SConcurrentHashMap[K, V](new ConcurrentHashMap[K, V]())
}
