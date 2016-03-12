package com.haskforce.ui

import javax.swing.ListCellRenderer

import com.haskforce.macros.ui.SListCellRendererWrapperMacro

@SListCellRendererWrapperMacro
class SListCellRendererWrapper[A](showItem: A => String) extends ListCellRenderer[A]
