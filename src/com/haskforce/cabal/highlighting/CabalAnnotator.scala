package com.haskforce.cabal.highlighting

import com.haskforce.cabal.lang.psi.CabalTypes._
import com.haskforce.cabal.lang.psi._
import com.intellij.lang.annotation.{AnnotationHolder, Annotator, HighlightSeverity}
import com.intellij.openapi.editor.colors.{EditorColorsManager, TextAttributesKey}
import com.intellij.psi.PsiElement

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

  private def setHighlighting(element: PsiElement, holder: AnnotationHolder, key: TextAttributesKey): Unit = {
    holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
      .enforcedTextAttributes(EditorColorsManager.getInstance.getGlobalScheme.getAttributes(key))
      .create()
  }
}
