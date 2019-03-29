package com.haskforce.features.intentions

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase
import com.haskforce.highlighting.annotation.external.SymbolImportProvider
import com.haskforce.macros.string.dedent

class AddToImportsTest
  extends HaskellLightPlatformCodeInsightFixtureTestCase("add-to-imports") {

  def testSimpleCreateImportNoModule(): Unit = {
    given(dedent("""
         main = when True $ print 1
         """))
      .whenAddingImport("Control.Monad", "when")
      .shouldBe(dedent("""
         import Control.Monad (when)

         main = when True $ print 1
         """))
  }

  def testSimpleCreateImportWithModule(): Unit = {
    given(dedent("""
         module Main where

         main = when True $ print 1
         """))
      .whenAddingImport("Control.Monad", "when")
      .shouldBe(dedent("""
         module Main where

         import Control.Monad (when)

         main = when True $ print 1
         """))
  }

  // Given a starting Haskell text body,
  private case class given(body: String) {
    // when adding an import (module, symbol)
    case class whenAddingImport(mod: String, sym: String) {
      // it should have the supplied new text body.
      def shouldBe(newBody: String): Unit = {
        myFixture.configureByText("A.hs", body)
        new AddToImports(sym).doAddImport(
          myFixture.getProject,
          myFixture.getFile,
          SymbolImportProvider.Result(mod, sym)
        )
        myFixture.checkResult(newBody)
      }
    }
  }
}
