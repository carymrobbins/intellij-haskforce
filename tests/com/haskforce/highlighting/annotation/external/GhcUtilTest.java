package com.haskforce.highlighting.annotation.external;

import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.testFramework.UsefulTestCase;
import org.junit.Assert;

import java.util.Arrays;

/**
 * Test class for GhcUtil, which should contain the actions common to GhcMod and GhcModi
 */
public class GhcUtilTest extends UsefulTestCase {

    public void testCanHandleSimpleOutput() {
        String ghcModTypeInfo = "24 31 24 45 \"Player\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.unsafeHandleTypeInfo(new VisualPosition (24, 33), new VisualPosition (24, 33), ghcModTypeInfo);
        Assert.assertEquals("Player", typeInfo);
    }

    public void testCanHandleTypeWithSpacesOutput() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.unsafeHandleTypeInfo(new VisualPosition (24, 49), new VisualPosition (24, 49), ghcModTypeInfo);
        Assert.assertEquals("(Player -> Bool) -> [Player] -> [Player]", typeInfo);
    }

    public void testCanHandleMultipleLines() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.unsafeHandleTypeInfo(new VisualPosition (24, 60),new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("[Player] -> [Player]", typeInfo);
    }

    public void testCanUseSelection() {
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.unsafeHandleTypeInfo(
                new VisualPosition(24, 5),
                new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("Player -> [Player] -> [Player]", typeInfo);
    }

    public void testTypeInfoParseError() {
        String ghcModTypeInfo = StringUtil.join(Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "         /src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
                "/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n");
        try {
            GhcUtil.handleTypeInfo(new VisualPosition(1, 1), new VisualPosition(1, 1), ghcModTypeInfo);
            fail("Expected TypeInfoParseException to be thrown.");
        } catch (GhcUtil.TypeInfoParseException e) {
            String userError = e.getUserError();
            assertNotNull(userError);
            assertEquals("/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)", userError);
        }
    }

    public void testTypeInfoUnknownError() {
        String ghcModTypeInfo = StringUtil.join(Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "some crazy unknown message"
        ), "\n");
        try {
            GhcUtil.handleTypeInfo(new VisualPosition(1, 1), new VisualPosition(1, 1), ghcModTypeInfo);
            fail("Expected TypeInfoParseException to be thrown.");
        } catch (GhcUtil.TypeInfoParseException e) {
            String userError = e.getUserError();
            assertNull(userError);
            assertEquals(ghcModTypeInfo, e.stdout);
        }
    }
}
