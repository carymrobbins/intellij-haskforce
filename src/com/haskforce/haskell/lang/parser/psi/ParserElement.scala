package com.haskforce.haskell.lang.parser.psi

import com.haskforce.HaskellLanguage
import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.{IElementType, IFileElementType}

sealed abstract class ParserElement(name: String)
  extends IElementType(name, HaskellLanguage.INSTANCE)

object Elements {

  case object HASKELL_FILE extends IFileElementType(
    "HASKELL_FILE", HaskellLanguage.INSTANCE)

  case object UNKNOWN extends ParserElement("UNKNOWN")
  case object MODULE_NAME extends ParserElement("MODULE_NAME")
  case object MODULE_DECL extends ParserElement("MODULE_DECL")

  case object IMPORT_MODULE extends ParserElement("IMPORT_MODULE")
  case object IMPORT_LIST extends ParserElement("IMPORT_LIST")
  case object IMPORT_STMT extends ParserElement("IMPORT_STMT")
  case object IMPORT_ALIAS extends ParserElement("IMPORT_ALIAS")
  case object IMPORT_EXPLICIT extends ParserElement("IMPORT_EXPLICIT")
  case object IMPORT_EXPLICITS extends ParserElement("IMPORT_EXPLICITS")
  case object IMPORT_HIDDEN extends ParserElement("IMPORT_HIDDEN")
  case object IMPORT_HIDDENS extends ParserElement("IMPORT_HIDDENS")

  case object FOREIGN_IMPORT extends ParserElement("FOREIGN_IMPORT")
  case object FOREIGN_EXPORT extends ParserElement("FOREIGN_EXPORT")

  case object TYPE_EXPR extends ParserElement("TYPE_EXPR")

  case object STRING_LIT extends ParserElement("STRING_LIT")

  case object QCONID extends ParserElement("QCONID")
}

object ElementFactory {

  // Hack for now
  class HaskellCompositeNode(node: ASTNode) extends ASTWrapperPsiElement(node) {
    override def toString: String = node.getElementType.toString
  }

  def createElement(node: ASTNode): PsiElement = node.getElementType match {
    case t: ParserElement => createElement(node, t)
    case t => throw new AssertionError(s"Unknown element type: $t")
  }

  def createElement(node: ASTNode, t: ParserElement): HaskellCompositeNode =
    // t match { ... }
    new HaskellCompositeNode(node)
}
