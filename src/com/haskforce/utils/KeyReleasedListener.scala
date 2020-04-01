package com.haskforce.utils

import java.awt.event.{KeyEvent, KeyListener}

abstract class KeyReleasedListener extends KeyListener {
  override def keyTyped(e: KeyEvent): Unit = ()
  override def keyPressed(e: KeyEvent): Unit = ()
}

object KeyReleasedListener {
  def apply(f: KeyEvent => Unit): KeyReleasedListener = (e: KeyEvent) => f(e)
}
