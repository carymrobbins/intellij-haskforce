package com.haskforce.importWizard.stack

import java.util

import scalaz.syntax.either._

import junit.framework.TestCase
import com.haskforce.macros.string.dedent

/** Tests for parsing stack.yaml files. */
class StackYamlTest extends TestCase {
  def testParseBoolean(): Unit = {
    val actual = StackYaml.fromString(dedent("""
      resolver: lts-5.0

      packages:
      - '.'

      extra-deps: []

      flags:
        alex:
          small_base: true

      extra-package-dbs: []
    """))

    val expected = StackYaml(packages("."))

    TestCase.assertEquals(expected.right, actual)
  }

  private def packages(ps: String*): util.List[StackYaml.Package] = {
    util.Arrays.asList(ps.map(StackYaml.Package): _*)
  }
}
