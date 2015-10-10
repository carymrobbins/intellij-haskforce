package com.haskforce.macros.ui

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Macro annotation to rewrite the class definition for SComboBox.
 * The problem is that JComboBox on JDK6 does not take type parameters, but the one for JDK7+ does.
 * This causes problems when targeting multiple JDKs with Scala since type parameters are not optional.
 */
class SComboBoxMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SComboBoxMacro.impl
}

object SComboBoxMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._ // Needed for quasiquotes
    Class.forName("javax.swing.JComboBox").getTypeParameters.length match {
      case 0 => q"""class SComboBox[E] extends JComboBox"""
      case 1 => q"""class SComboBox[E] extends JComboBox[E]"""
    }
  }
}
