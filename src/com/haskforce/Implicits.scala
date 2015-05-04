package com.haskforce

import java.awt.event._
import javax.swing.event.{DocumentListener, DocumentEvent}

import com.intellij.openapi.util.Condition

object Implicits {
  implicit def funToRunnable(block: => Unit): Runnable = new Runnable { def run() = block }

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

  implicit class BooleanToOption(b: Boolean) {
    def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }

  /**
   * Provides a .nullMap() method for Option which wraps potentially null results in an Option.
   * Example:
   *   foo.getBar().getBaz() <- one of these might be null
   *   Option(foo).nullMap(_.getBar()).nullMap(_.getBaz()) <- returns None if any value was null.
   */
  // TODO: Should we create Scala interfaces around the Java ones to replace null with Option entirely?
  implicit class ToOptionNullMap[A](o: Option[A]) {
    def nullMap[B](f: A => B): Option[B] = o.flatMap(a => Option(f(a)))
  }
}
