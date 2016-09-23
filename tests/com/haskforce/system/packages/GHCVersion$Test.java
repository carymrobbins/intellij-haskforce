package com.haskforce.system.packages;

import org.junit.Test;
import scala.Option;

import static org.junit.Assert.*;

/**
 */
public class GHCVersion$Test {
    @Test
    public void getGHCVersion8() throws Exception {
        Option<GHCVersion> ghcVersion = GHCVersion.getGHCVersion("8.0.1");
        assertTrue(ghcVersion.isDefined());
        assertEquals(ghcVersion.get(), GHCVersion.apply(8, 0, 1));
    }

    @Test
    public void getGHCVersion7() throws Exception {
        Option<GHCVersion> ghcVersion = GHCVersion.getGHCVersion("7.10.3");
        assertTrue(ghcVersion.isDefined());
        assertEquals(ghcVersion.get(), GHCVersion.apply(7, 10, 3));
    }
}