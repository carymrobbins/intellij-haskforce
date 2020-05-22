package com.haskforce.lsp.annotator

import java.util.regex.Pattern

import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.Annotation
import org.wso2.lsp4intellij.contributors.annotator.LSPAnnotator

class HaskForceLSPAnnotator extends LSPAnnotator {

  override def modifyCreatedAnnotation(annotation: Annotation): Unit = {
    GhcMessageType.of(annotation.getMessage).foreach {
      case GhcMessageType.NotInScope =>
        annotation.setHighlightType(ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
    }
  }
}

// TODO: Not a great place for this to live, should be unified with the regex stuff in GhcMod.java
sealed trait GhcMessageType
object GhcMessageType {

  def of(s: String): Option[GhcMessageType] = {
    if (isNotInScope(s)) return Some(NotInScope)
    None
  }

  case object NotInScope extends GhcMessageType
  private val notInScope = Pattern.compile("(?i)not in scope")
  private def isNotInScope(s: String) = notInScope.matcher(s).find()
}
