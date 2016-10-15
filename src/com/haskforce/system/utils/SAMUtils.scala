package com.haskforce.system.utils

import java.awt.event.{ItemEvent, ItemListener}
import javax.swing.{Icon, JList}
import javax.swing.event.PopupMenuEvent

import com.intellij.openapi.util.{Computable, Condition}
import com.intellij.ui.PopupMenuListenerAdapter

import com.haskforce.system.ui.SListCellRendererWrapper

/** Utilities for creating instances for Single Abstract Method classes (or the like). */
object SAMUtils {

  def runnable(f: => Unit) = new Runnable {
    override def run(): Unit = f
  }

  def computable[A](f: => A) = new Computable[A] {
    override def compute(): A = f
  }

  def condition[A](f: A => Boolean) = new Condition[A] {
    override def value(t: A): Boolean = f(t)
  }

  def popupMenuWillBecomeVisible(f: PopupMenuEvent => Unit) = new PopupMenuListenerAdapter {
    override def popupMenuWillBecomeVisible(e: PopupMenuEvent): Unit = f(e)
  }

  def itemListener(f: ItemEvent => Unit) = new ItemListener {
    override def itemStateChanged(e: ItemEvent): Unit = f(e)
  }

  def listCellRenderer[A](f: A => String) = new SListCellRendererWrapper[A](f)

  def ijFunction[A, B](f: A => B): com.intellij.util.Function[A, B] = new com.intellij.util.Function[A, B] {
    override def fun(param: A): B = f(param)
  }

  object guava {
    def predicate[A](f: A => Boolean) = new com.google.common.base.Predicate[A] {
      override def apply(t: A): Boolean = f(t)
    }
  }
}
