package com.haskforce.ui

import javax.swing.JComboBox

import com.haskforce.macros.ui.SComboBoxMacro

/**
 * Scala-friendly JComboBox; see [[SComboBoxMacro]] for more details.
 */
@SComboBoxMacro class SComboBox[E] extends JComboBox[E]
