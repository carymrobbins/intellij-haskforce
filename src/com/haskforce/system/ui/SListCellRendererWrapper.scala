package com.haskforce.system.ui

import javax.swing.JList

import com.intellij.ui.ListCellRendererWrapper

/** Create a ListCellRenderer from a simple function A => String */
class SListCellRendererWrapper[A](showItem: A => String) extends ListCellRendererWrapper[A] {
  override def customize(jList: JList[_], t: A, i: Int, b: Boolean, b1: Boolean): Unit = {
    if (t == null) return
    setText(showItem(t))
  }
}
