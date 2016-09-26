package com.haskforce.tools.cabal.highlighting

import com.intellij.lang.annotation.{AnnotationHolder, Annotator}
import com.intellij.openapi.editor.colors.{EditorColorsManager, TextAttributesKey}
import com.intellij.psi.PsiElement

import com.haskforce.tools.cabal.lang.psi._
import com.haskforce.tools.cabal.lang.psi.CabalTypes._

class CabalAnnotator extends Annotator {

  def annotate(el: PsiElement, h: AnnotationHolder): Unit = {
    el.getNode.getElementType match {
      case _: CabalFieldKeyTokenType => setHighlighting(el, h, CabalSyntaxHighlighter.KEY)
      case _: CabalStanzaKeyTokenType => setHighlighting(el, h, CabalSyntaxHighlighter.CONFIG)
      case _: CabalStanzaArgTokenType => setHighlighting(el, h, CabalSyntaxHighlighter.CONFIG)
      case LBRACE | RBRACE => setHighlighting(el, h, CabalSyntaxHighlighter.COLON)
      case _ => // noop
    }
  }

  private def setHighlighting(element: PsiElement, holder: AnnotationHolder, key: TextAttributesKey) {
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(
      EditorColorsManager.getInstance.getGlobalScheme.getAttributes(key)
    )
  }
}
