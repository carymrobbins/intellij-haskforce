package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.test.AssertMixin
import com.haskforce.utils.NonEmptySet
import com.intellij.testFramework.fixtures.BasePlatformTestCase

class PackageConfigTest extends BasePlatformTestCase with AssertMixin {

  def test00001(): Unit = {
    cabalFileShouldBe("foo", PackageConfig(
      name = "foo",
      components = List(
        PackageConfig.Component(
          typ = PackageConfig.Component.Type.Library,
          name = "foo",
          hsSourceDirs = NonEmptySet("src"),
          mainIs = None,
          dependencies = Set(
            "aeson",
            "base",
            "bytestring",
            "containers",
            "filepath",
            "text",
            "time",
            "uuid"
          ).map(PackageConfig.Dependency(_, None)),
          extensions = Set(
            "BlockArguments",
            "ConstraintKinds",
            "DataKinds",
            "DeriveAnyClass"
          )
        ),
        PackageConfig.Component(
          typ = PackageConfig.Component.Type.TestSuite,
          name = "foo-tests",
          hsSourceDirs = NonEmptySet("test"),
          mainIs = Some("Main.hs"),
          dependencies = Set(
            "aeson",
            "aeson-qq",
            "base",
            "hspec"
          ).map(PackageConfig.Dependency(_, None)),
          extensions = Set(
            "BlockArguments",
            "ConstraintKinds",
            "DataKinds"
          )
        )
      )
    ))
  }

  private def testName: String = getTestName(false)

  private def cabalFileShouldBe(name: String, c: PackageConfig): Unit = {
    assertEquals(c, cabalFileFixture(name))
  }

  private def cabalFileFixture(name: String): PackageConfig = {
    namedFixture(s"$testName/$name.cabal")
  }

  private def namedFixture(relPath: String): PackageConfig = {
    val file = new File(s"tests/gold/project/externalSystem/stack/packageConfig/$relPath")
    PackageConfig.fromFile(file) match {
      case Right(o) => assertSome(o)
      case Left(e) => throw new AssertionError(s"Failed to parse fixture: $file", e)
    }
  }
}
