package com.haskforce.lsp.annotator

import java.util.regex.{Matcher, Pattern}

import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.Annotation
import org.wso2.lsp4intellij.contributors.annotator.LSPAnnotator

class HaskForceLSPAnnotator extends LSPAnnotator {

  override def modifyCreatedAnnotation(annotation: Annotation): Unit = {
    GhcMessageType.of(annotation.getMessage).foreach {
      case GhcMessageType.NotInScope =>
        annotation.setHighlightType(ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
      case GhcMessageType.CouldNotFindModule =>
        annotation.setHighlightType(ProblemHighlightType.LIKE_UNKNOWN_SYMBOL)
    }
  }
}

// TODO: Not a great place for this to live, should be unified with the regex stuff in GhcMod.java
sealed trait GhcMessageType
object GhcMessageType {

  def of(s: String): Option[GhcMessageType] = {
    ALL.foreach { typ =>
      if (typ.matches(s)) return Some(typ)
    }
    None
  }

  case object NotInScope extends Base("(?i)not in scope", _.find())
  case object CouldNotFindModule extends Base("(?i)could not find module", _.find())

  private val ALL = List[Base](
    NotInScope,
    CouldNotFindModule
  )

  sealed abstract class Base(
    regex: String,
    check: Matcher => Boolean
  ) extends GhcMessageType {
    private val pattern = Pattern.compile(regex)
    def matches(s: String): Boolean = check(pattern.matcher(s))
  }
}
