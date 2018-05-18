package com.haskforce;

import com.haskforce.core.HaskForceCoreMeta;
import junit.framework.TestCase;

public class HaskForceMetaTest extends TestCase {

  public void testGetHaskForceCoreDependencyVersion() {
    assertEquals(
      HaskForceCoreMeta.VERSION,
      HaskForceMeta.CORE_VERSION
    );
  }
}
