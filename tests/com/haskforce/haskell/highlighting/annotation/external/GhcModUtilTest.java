package com.haskforce.haskell.highlighting.annotation.external;

import com.haskforce.tools.ghcmod.GhcModUtil;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.testFramework.UsefulTestCase;
import org.junit.Assert;

/**
 * Test class for GhcModUtil, which should contain the actions common to GhcMod and GhcModi
 */
public class GhcModUtilTest extends UsefulTestCase {

    public void testCanHandleSimpleOutput() {
        String ghcModTypeInfo = "24 31 24 45 \"Player\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcModUtil.unsafeHandleTypeInfo(new VisualPosition (24, 33), new VisualPosition (24, 33), ghcModTypeInfo);
        Assert.assertEquals("Player", typeInfo);
    }

    public void testCanHandleTypeWithSpacesOutput() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcModUtil.unsafeHandleTypeInfo(new VisualPosition (24, 49), new VisualPosition (24, 49), ghcModTypeInfo);
        Assert.assertEquals("(Player -> Bool) -> [Player] -> [Player]", typeInfo);
    }

    public void testCanHandleMultipleLines() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcModUtil.unsafeHandleTypeInfo(new VisualPosition (24, 60),new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("[Player] -> [Player]", typeInfo);
    }

    public void testCanUseSelection() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcModUtil.unsafeHandleTypeInfo(
                new VisualPosition(24, 5),
                new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("Player -> [Player] -> [Player]", typeInfo);
    }
}
