package com.haskforce.cabal

import com.haskforce.cabal.settings.{CabalComponentType, AddCabalPackageOptions}
import com.haskforce.cabal.settings.ui.AddCabalPackageUtil
import com.intellij.testFramework.UsefulTestCase

/**
 * Test to ensure proper behavior of the AddCabalPackage data types.
 */
class AddCabalPackageTest extends UsefulTestCase {
  val defaultOptions = AddCabalPackageOptions(
    maybeModule = None,
    packageName = "my-package",
    packageVersion = "0.1",
    buildType = CabalComponentType.Library,
    rootDir = "path/to/my-package",
    sourceDir = "src",
    cabalVersion = ">=1.20",
    license = None,
    author = None,
    email = None,
    homepage = None,
    synopsis = None,
    category = None,
    language = "Haskell2010",
    generateComments = true
  )

  def testBuildArgsIgnoresEmptyStrings(): Unit = {
    val args = AddCabalPackageUtil.buildArgs(defaultOptions.copy(cabalVersion = ""))
    assertEmpty(args.filter(_.contains("cabal-version")))
  }

  def assertEmpty(xs: Seq[_]): Unit = {
    assert(xs.isEmpty, s"Expected empty sequence, got $xs")
  }
}
