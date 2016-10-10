package com.haskforce.system.packages

import com.haskforce.test.AssertMixin
import junit.framework.TestCase

class GHCVersionTest extends TestCase with AssertMixin {

  def testGetGHCVersion8() {
    val ghcVersion: Option[GHCVersion] = GHCVersion.getGHCVersion("8.0.1")
    assertTrue(ghcVersion.isDefined)
    assertEquals(ghcVersion.get, GHCVersion.apply(8, 0, 1))
  }

  def testGetGHCVersion7() {
    val ghcVersion: Option[GHCVersion] = GHCVersion.getGHCVersion("7.10.3")
    assertTrue(ghcVersion.isDefined)
    assertEquals(ghcVersion.get, GHCVersion.apply(7, 10, 3))
  }

}
