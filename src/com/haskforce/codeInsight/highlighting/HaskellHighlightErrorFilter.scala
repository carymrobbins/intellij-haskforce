package com.haskforce.codeInsight.highlighting

import com.haskforce.HaskellLanguage
import com.haskforce.utils.SystemPropertyUtil
import com.intellij.codeInsight.highlighting.HighlightErrorFilter
import com.intellij.psi.PsiErrorElement

/** This class is used as an extension point to conditionally
 * disable highlighting PSI parse errors in the editor.
 * Ideally, most users will prefer to get these kinds of errors
 * from external tools.
 *
 * The parser might also show false negatives
 * that actually do parse, and our parser doesn't deal with it properly.
 * We'd rather not confuse the user with false errors as the parser is
 * mostly useful for things like jump-to-definition, find-references, etc.
 *
 * Enabling the display of parse errors is likely more useful to
 * a developer of HaskForce rather than someone who simply uses it.
 * So by default, we disable highlighting error elements.
 *
 * To enable highlighting parse errors in the editor, add
 * the following line to your idea.properties file -
 * {{{
 *   com.haskforce.parser.error.highlight=true
 * }}}
 */
final class HaskellHighlightErrorFilter extends HighlightErrorFilter {

  override def shouldHighlightErrorElement(element: PsiErrorElement): Boolean = (
    // Disable highlighting of error elements if the element is in
    // a Haskell file AND we have not explicitly opted to show them.
    HaskellLanguage.INSTANCE != element.getLanguage
      || HaskellHighlightErrorFilter.HIGHLIGHT_PARSE_ERROR
  )
}

object HaskellHighlightErrorFilter {
  private val HIGHLIGHT_PARSE_ERROR =
    SystemPropertyUtil.parseBoolWithDefault(
      "com.haskforce.parser.error.highlight", default = false)
}
