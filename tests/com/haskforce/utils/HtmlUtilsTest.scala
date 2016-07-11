package com.haskforce.utils

import junit.framework.TestCase

import com.haskforce.highlighting.annotation.external.GhcModi
import com.haskforce.macros.string.dedent
import com.haskforce.test.AssertMixin

class HtmlUtilsTest extends TestCase with AssertMixin {

  def testStripHtmlTags(): Unit = {
    HtmlUtils.stripTags(GhcModi.KILLING_MESSAGE_PREFIX + "foo") === dedent("""
      Killing ghc-modi due to process failure.

      You can restart it using Tools > Restart ghc-modi

      foo
    """)
  }

}
