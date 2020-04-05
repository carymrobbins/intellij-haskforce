package com.haskforce.utils

import java.io.StringReader
import javax.swing.text.html.HTMLEditorKit
import javax.swing.text.html.parser.ParserDelegator

object HtmlUtils {

  /** Strips tags from HTML string, replacing line break tags with newlines. */
  def stripTags(s: String): String = {
    // The HTMLEditorKit doesn't seem to replace <br/> with newlines,
    // so we first inject "%%NEWLINE%%" tokens for each <br/>, strip
    // remaining tags with our parser, then replace our newline tokens
    // with actual newlines.
    NEWLINE_TOKEN_REGEX.replaceAllIn(
      (new MyHtmlParser).parse(BR_REGEX.replaceAllIn(s, NEWLINE_TOKEN)),
      "\n"
    )
  }

  private val BR_REGEX = """(?i)<br\s*/?\s*>""".r
  private val NEWLINE_TOKEN = "%%NEWLINE%%"
  private val NEWLINE_TOKEN_REGEX = NEWLINE_TOKEN.r

  /** Adapted from http://stackoverflow.com/a/455196/1313611 */
  private final class MyHtmlParser extends HTMLEditorKit.ParserCallback {

    private[this] var buf: StringBuffer = null

    def parse(s: String): String = {
      buf = new StringBuffer()
      val d = new ParserDelegator()
      d.parse(new StringReader(s), this, /* ignoreCharSet */ true)
      buf.toString
    }

    override def handleText(data: Array[Char], pos: Int): Unit = {
      buf.append(data)
      ()
    }
  }
}
