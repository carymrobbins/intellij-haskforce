package com.haskforce.spellchecker

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase
import com.intellij.spellchecker.inspections.SpellCheckingInspection

/**
 * Tests for the Haskell spellchecker.
 */
class HaskellSpellcheckingTest extends HaskellLightPlatformCodeInsightFixtureTestCase("spellchecker") {
  override protected def setUp(): Unit = {
    super.setUp()
    myFixture.enableInspections(new SpellCheckingInspection)
  }

  override def isWriteActionRequired: Boolean = false

  private def doTest(): Unit = {
    myFixture.testHighlighting(false, false, true, getTestName(false) + ".hs")
  }

  def testSpelling() = doTest()
}
