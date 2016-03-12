package com.haskforce.macros.ui

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * Macro annotation to rewrite the class definition for SListCellRenderer.
  * This macro serves the same purpose as SComboBoxMacro, so see its javadoc for details.
  */
class SListCellRendererWrapperMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SListCellRendererWrapperMacro.impl
}

object SListCellRendererWrapperMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._ // Needed for quasiquotes

    val qualJList = Select(Select(Ident(TermName("javax")), TermName("swing")), TypeName("JList"))
    val qualListCellRenderer = Select(Select(Ident(TermName("javax")), TermName("swing")), TypeName("ListCellRenderer"))

    def withTypeParam(cls: Tree, t: String) = AppliedTypeTree(cls, List(Ident(TypeName(t))))

    val numTypeParams = Class.forName("javax.swing.ListCellRenderer").getTypeParameters.length

    val clsListCellRenderer = numTypeParams match {
      case 0 => qualListCellRenderer
      case 1 => withTypeParam(qualListCellRenderer, "A")
    }

    val clsJList_ = numTypeParams match {
      case 0 => qualJList
      case 1 =>
        ExistentialTypeTree(
          AppliedTypeTree(qualJList, List(Ident(TypeName("_$1")))), List(
            TypeDef(
              Modifiers(Flag.DEFERRED | Flag.SYNTHETIC), TypeName("_$1"), List(), TypeBoundsTree(
                EmptyTree, EmptyTree
              )
            )
          )
        )
    }

    val clsJListBound = numTypeParams match {
      case 0 => qualJList
      case 1 =>
        ExistentialTypeTree(
          AppliedTypeTree(qualJList, List(Ident(TypeName("_$1")))), List(
            TypeDef(
              Modifiers(Flag.DEFERRED | Flag.SYNTHETIC), TypeName("_$1"), List(),
              TypeBoundsTree(EmptyTree, Ident(TypeName("A")))
            )
          )
        )
    }

    val clsElem = numTypeParams match {
      case 0 => Ident(TypeName("Any"))
      case 1 => Ident(TypeName("A"))
    }

    q"""
      class SListCellRendererWrapper[A](showItem: A => String) extends $clsListCellRenderer {

        override def getListCellRendererComponent
            (list: $clsJListBound, value: $clsElem, index: Int, isSelected: Boolean, cellHasFocus: Boolean)
            : java.awt.Component = {
          wrapper.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus)
        }

        object wrapper extends com.intellij.ui.ListCellRendererWrapper[A] {
          override def customize
              (list: $clsJList_, value: A, index: Int, selected: Boolean, hasFocus: Boolean)
              : Unit = {
            Option(value).foreach(v => setText(showItem(v)))
          }
        }
      }
    """
  }
}
