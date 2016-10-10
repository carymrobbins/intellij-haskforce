package com.haskforce.tools.stack.importWizard

import java.util

import com.haskforce.importWizard.stack.StackYaml
import com.haskforce.macros.string.dedent
import junit.framework.TestCase

import scalaz.\/
import scalaz.syntax.either._

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

  def testPackagesDefaultsToRoot(): Unit = {
    val actual = StackYaml.fromString(dedent("""
      resolver: lts-3.14
      image:
        container:
          base: "fpco/ubuntu-with-libgmp:14.04"
          entrypoints:
            - stack
    """))

    val expected = StackYaml(packages("."))

    assertRight(expected, actual)
  }

  def testLocationPackages(): Unit = {
    val actual = StackYaml.fromString(dedent("""
      resolver: lts-5.2
      packages:
      - '.'
      - location:
          git: https://github.com/haskoin/haskoin.git
          commit: 7ccccb0a35ad959e4fcef91748c9d3653e39cb2e
        extra-dep: true
        subdirs:
          - haskoin-core
      extra-deps:
      - ed25519-0.0.5.0
      - secp256k1-0.4.4
      - spawn-0.3
      - murmur3-1.0.1
      - pbkdf-1.1.1.1
      flags: {}
      extra-package-dbs: []
    """))

    val expected = StackYaml(packages("."))

    assertRight(expected, actual)
  }

  private def packages(ps: String*): util.List[StackYaml.Package] = {
    util.Arrays.asList(ps.map(StackYaml.Package): _*)
  }

  private def assertRight[A](expected: A, actual: _ \/ A) = {
    TestCase.assertEquals(expected.right, actual)
  }
}
