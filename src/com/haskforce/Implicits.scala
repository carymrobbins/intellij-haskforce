package com.haskforce

import java.awt.event._
import java.util
import java.util.Comparator
import java.util.concurrent.Callable
import javax.swing.event.{DocumentEvent, DocumentListener}

import scala.collection.JavaConverters._
import scalaz.\/

import com.intellij.openapi.util.{Computable, Condition}

object Implicits {

  implicit final class RichOption[A](val underlying: Option[A]) extends AnyVal {
    def disj[L](left: L): L \/ A = underlying match {
      case None => \/.left(left)
      case Some(a) => \/.right(a)
    }
  }

  implicit final class Fun0[A](val underlying: () => A) extends AnyVal {
    def toRunnable(implicit ev: A =:= Unit): Runnable = new Runnable {
      override def run(): Unit = underlying()
    }

    def toCallable: Callable[A] = new Callable[A] {
      override def call(): A = underlying()
    }

    def toComputable: Computable[A] = new Computable[A] {
      override def compute(): A = underlying()
    }
  }

  implicit def funToRunnable(block: () => Unit): Runnable = block.toRunnable

  implicit def funToComputable[A](block: () => A): Computable[A] = block.toComputable

  implicit final class Fun1[A, B](val underlying: A => B) extends AnyVal {
    def toIJFunction: com.intellij.util.Function[A, B] = new com.intellij.util.Function[A, B] {
      override def fun(param: A): B = underlying(param)
    }
  }

  implicit def funToIJFunction[A, B](f: A => B): com.intellij.util.Function[A, B] = f.toIJFunction

  implicit def funToItemListener(f: ItemEvent => Unit): ItemListener = new ItemListener {
    override def itemStateChanged(e: ItemEvent): Unit = f(e)
  }

  implicit class FunToKeyListener(f: KeyEvent => Unit) extends KeyAdapter {
    override def keyReleased(e: KeyEvent): Unit = f(e)
  }

  implicit def funToActionListener(f: ActionEvent => Unit): ActionListener = new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = f(e)
  }

  implicit class FunToDocumentListener(f: DocumentEvent => Unit) extends DocumentListener {
    override def insertUpdate(e: DocumentEvent): Unit = f(e)
    override def changedUpdate(e: DocumentEvent): Unit = f(e)
    override def removeUpdate(e: DocumentEvent): Unit = f(e)
  }

  implicit def funToCondition[A](f: A => Boolean): Condition[A] = new Condition[A] {
    override def value(t: A): Boolean = f(t)
  }

  implicit def funToComparator[A](f: (A, A) => Int): Comparator[A] = new Comparator[A] {
    override def compare(o1: A, o2: A): Int = f(o1, o2)
  }

  implicit class FunToConsumer[A](f: A => Unit) extends com.intellij.util.Consumer[A] {
    override def consume(a: A): Unit = f(a)
  }

  implicit class BooleanToOption(b: Boolean) {
    def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }

  implicit class RichJavaCollection[A](val underlying: util.Collection[A]) {
    def foreach[U](f: A => U): Unit = underlying.iterator().asScala.foreach(f)
  }

  /**
   * Provides a .nullMap() method for Option which wraps potentially null results in an Option.
   * Example:
   *   foo.getBar().getBaz() <- one of these might be null
   *   Option(foo).nullMap(_.getBar()).nullMap(_.getBaz()) <- returns None if any value was null.
   */
  // TODO: Should we create Scala interfaces around the Java ones to replace null with Option entirely?
  implicit class ToOptionNullMap[A](private val o: Option[A]) extends AnyVal {
    def nullMap[B](f: A => B): Option[B] = o.flatMap(a => Option(f(a)))
  }
}
