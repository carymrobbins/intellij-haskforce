package com.haskforce.highlighting.annotation.external;

import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.editor.VisualPosition;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by kasper on 12/24/14.
 */
public class GhcUtilTest {

    @Test
    public void canHandleSimpleOutput (){
        String ghcModTypeInfo = "24 31 24 45 \"Player\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.handleTypeInfo(new VisualPosition (24, 33), new VisualPosition (24, 33), ghcModTypeInfo);
        Assert.assertEquals("Player", typeInfo);
    }

    @Test
    public void canHandleTypeWithSpacesOutput (){
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.handleTypeInfo(new VisualPosition (24, 49), new VisualPosition (24, 49), ghcModTypeInfo);
        Assert.assertEquals("(Player -> Bool) -> [Player] -> [Player]", typeInfo);
    }

    @Test
    public void canHandleMultipleLines (){
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo = GhcUtil.handleTypeInfo(new VisualPosition (24, 60),new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("[Player] -> [Player]", typeInfo);
    }

    @Test
    public void canUseSelection(){
        String ghcModTypeInfo =
                "24 48 24 59 \"(Player -> Bool) -> [Player] -> [Player]\"\n" +
                "24 48 24 109 \"[Player] -> [Player]\"\n" +
                "24 1 25 52 \"Player -> [Player] -> [Player]\"";
        String typeInfo;
        typeInfo = GhcUtil.handleTypeInfo(
                new VisualPosition(24, 5),
                new VisualPosition (24, 60), ghcModTypeInfo);
        Assert.assertEquals("Player -> [Player] -> [Player]", typeInfo);
    }
}
