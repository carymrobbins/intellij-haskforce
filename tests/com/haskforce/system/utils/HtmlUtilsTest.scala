package com.haskforce.system.utils

import junit.framework.TestCase
import com.haskforce.macros.string.dedent
import com.haskforce.test.AssertMixin
import com.haskforce.tools.ghcmod.modi.GhcModi

class HtmlUtilsTest extends TestCase with AssertMixin {

  def testStripHtmlTags(): Unit = {
    HtmlUtils.stripTags(GhcModi.KILLING_MESSAGE_PREFIX + "foo") === dedent("""
      Killing ghc-modi due to process failure.

      You can restart it using Tools > Restart ghc-modi

      foo
    """)
  }

}
