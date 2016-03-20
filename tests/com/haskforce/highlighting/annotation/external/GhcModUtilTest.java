package com.haskforce.highlighting.annotation.external;

import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.testFramework.UsefulTestCase;
import org.junit.Assert;

import java.util.Arrays;

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
