package com.haskforce.spellchecker

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase
import com.intellij.spellchecker.inspections.SpellCheckingInspection

import scala.collection.JavaConverters._

/** Tests for the Haskell spellchecker. */
class HaskellSpellcheckingTest extends HaskellLightPlatformCodeInsightFixtureTestCase("spellchecker") {

  def testUnique() = {
    doTest()
    assertUniqueElements()
  }

  def testComments() = doTest()

  def testOnlyDefinitionNodes() = doTest()

  override protected def setUp(): Unit = {
    super.setUp()
    myFixture.enableInspections(new SpellCheckingInspection)
  }

  override def isWriteActionRequired: Boolean = false

  private def doTest(): Unit = {
    myFixture.testHighlighting(false, false, true, getTestName(false) + ".hs")
    ()
  }

  /**
   * Ensures that all typos found are unique by element. It seems that
   * [[com.intellij.spellchecker.tokenizer.SpellcheckingStrategy.TEXT_TOKENIZER]]
   * causes duplicate warnings for the same typo, so this assertion ensures that
   * doesn't happen.
   *
   * Note that this must be called _after_ [[myFixture.testHighlighting]] which
   * tells [[myFixture.doHighlighting]] which file we're asserting for.
   *
   */
  private def assertUniqueElements(): Unit = {
    val duplicates = myFixture.doHighlighting().asScala.groupBy(_.getText).collect {
      case (k, v) if v.length > 1 => (k, v.length)
    }
    assert(duplicates.isEmpty, s"Duplicate warnings found: $duplicates")
  }
}
