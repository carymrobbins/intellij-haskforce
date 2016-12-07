package com.haskforce.test

import java.io.File
import java.util

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.language.existentials

import com.intellij.testFramework.UsefulTestCase
import junit.framework.{AssertionFailedError, TestCase}
import org.jetbrains.annotations.Nullable

/** Collection of assertion helpers to mix into test cases */
trait AssertMixin {

  def fail(message: String): Unit = TestCase.fail(message)

  def assertTrue(c: Boolean): Unit = TestCase.assertTrue(c)

  def assertTrue(msg: String, c: Boolean): Unit = TestCase.assertTrue(msg, c)

  def assertFalse(c: Boolean): Unit = TestCase.assertFalse(c)

  def assertFalse(msg: String, c: Boolean): Unit = TestCase.assertFalse(msg, c)

  def assertEquals(message: String, expected: Any, actual: Any): Unit = {
    TestCase.assertEquals(message, expected, actual)
  }

  def assertEquals(expected: Any, actual: Any): Unit = TestCase.assertEquals(expected, actual)

  def assertNotEquals(x: Any, y: Any): Unit = {
    if (x == y) fail(s"$x == $y")
  }

  def assertNotNull(o: Any): Unit = TestCase.assertNotNull(o)

  def assertNotEmpty(s: String): Unit = {
    assertNotNull(s)
    assertFalse(s"String not empty: $s", s.isEmpty)
  }

  def assertSameElements[A]
      (message: String,
       collection: util.Collection[_ <: A],
       expected: util.Collection[A])
      : Unit
      = UsefulTestCase.assertSameElements(message, collection, expected)

  def assertSameElements[A](collection: util.Collection[_ <: A], expected: A*): Unit = {
    UsefulTestCase.assertSameElements(collection, expected: _*)
  }

  def assertSameElements[A](actual: Array[A], expected: A*): Unit = {
    UsefulTestCase.assertSameElements(util.Arrays.asList(actual), expected)
  }

  def assertEmpty(array: Array[AnyRef]): Unit = UsefulTestCase.assertEmpty(array)

  def assertEmpty(collection: util.Collection[_]): Unit = {
    UsefulTestCase.assertEmpty(collection)
  }

  /** Adapted from UsefulTestCase.assertInstanceOf */
  def assertInstanceOf[A : Manifest](@Nullable message: String, o: Any): A = {
    val c = manifest[A].runtimeClass
    val prefixed = maybePrefix(Option(message))

    TestCase.assertNotNull(
      prefixed(s"Expected instance of: ${c.getName} actual: null"),
      o
    )
    TestCase.assertTrue(
      prefixed(s"Expected instance of: ${c.getName} actual: ${o.getClass.getName}"),
      c.isInstance(o)
    )
    o.asInstanceOf[A]
  }

  def assertInstanceOf[A : Manifest](@Nullable o: Any): A = assertInstanceOf(null, o)

  def assertSome[A](o: Option[A]): A = {
    o match {
      case None | Some(null) => throw new AssertionFailedError(s"Expected Some, got $o")
      case Some(a) => a
    }
  }

  def assertSomeWith[A](o: Option[A])(f: A => Boolean): A = assertPredicate(assertSome(o))(f)

  def assertPredicate[A](a: A)(f: A => Boolean): A = {
    assertTrue(s"Failed to match predicate: $a", f(a))
    a
  }

  def assertFileExists(s: String): File = {
    val file = new File(s)
    assertTrue(s"File '$s' does not exist", file.exists())
    file
  }

  def assertExecutable(s: String): File = {
    val file = assertFileExists(s)
    assertTrue(s"File '$s' is not executable", file.canExecute)
    file
  }

  /** Attempts an assertion at repeated intervals until timeout. */
  def pollAssert
      (interval: FiniteDuration, timeout: FiniteDuration)
      (block: FiniteDuration => Unit)
      : Unit = {

    val startTime = System.nanoTime().nanos

    @tailrec
    def runBlock(blockTime: FiniteDuration): Unit = {
      try {
        block(blockTime - startTime)
      } catch {
        case e: AssertionError =>
          val now = System.nanoTime().nanos
          if (now > startTime + timeout) {
            throw new AssertionError(s"${e.getMessage} (tried for ${timeout.toMillis} millis)", e)
          }
          Thread.sleep(interval.toMillis)
          runBlock(System.nanoTime().nanos)
      }
    }

    runBlock(startTime)
  }

  /** Optionally prefix a string with another string. */
  def maybePrefix(prefix: Option[String], s: String): String = {
    prefix.map(_ + ": ").getOrElse("") + s
  }

  /** Curried version to simplify building a curried function with the prefix. */
  def maybePrefix(prefix: Option[String]): String => String = { s: String =>
    maybePrefix(prefix, s)
  }

  implicit class RichAssertions[A](val underlying: A) {
    def ===(other: A): Unit = assertEquals(other, underlying)
  }
}
