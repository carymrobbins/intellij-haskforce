package com.haskforce.settings;

import com.haskforce.HaskellIcons;
import com.haskforce.highlighting.HaskellSyntaxHighlighter;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import gnu.trove.THashMap;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

public class HaskellColorSettingsPage implements ColorSettingsPage {
    private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[] {
            new AttributesDescriptor("Pragma", HaskellSyntaxHighlighter.PRAGMA),
            new AttributesDescriptor("Reserved IDs", HaskellSyntaxHighlighter.RESERVEDID),
            new AttributesDescriptor("Constructor", HaskellSyntaxHighlighter.CONID),
            new AttributesDescriptor("Variable", HaskellSyntaxHighlighter.VARID),
            new AttributesDescriptor("Infix Function", HaskellSyntaxHighlighter.INFIXVARID),
            new AttributesDescriptor("Symbol", HaskellSyntaxHighlighter.VARSYM),
            new AttributesDescriptor("Special", HaskellSyntaxHighlighter.SPECIAL),
            new AttributesDescriptor("String", HaskellSyntaxHighlighter.STRING),
            new AttributesDescriptor("Integer", HaskellSyntaxHighlighter.INTEGER),
            new AttributesDescriptor("Float", HaskellSyntaxHighlighter.FLOAT),
            new AttributesDescriptor("Char", HaskellSyntaxHighlighter.CHAR),
            new AttributesDescriptor("Line Comment", HaskellSyntaxHighlighter.COMMENT),
            new AttributesDescriptor("Block Comment", HaskellSyntaxHighlighter.NCOMMENT),
            new AttributesDescriptor("Doc Comment", HaskellSyntaxHighlighter.HADDOCK),
            new AttributesDescriptor("Escape", HaskellSyntaxHighlighter.ESCAPE),
            new AttributesDescriptor("Quasi Quotes", HaskellSyntaxHighlighter.QQTEXT),
    };

    @Nullable
    @Override
    public Icon getIcon() {
        return HaskellIcons.FILE;
    }

    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return new HaskellSyntaxHighlighter();
    }

    @NotNull
    @Override
    public String getDemoText() {
        return  "{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}\n" +
                "<ri>module</ri> Example <sp>(</sp><vi>foo</vi><sp>,</sp> <vi>bar</vi><sp>)</sp> <ri>where</ri>\n" +
                "\n" +
                "<ri>import</ri> Control.Monad <ri>as</ri> M\n" +
                "<ri>import</ri> Control.Monad <sp>(</sp><vi>liftM2</vi><sp>)</sp>\n" +
                "<ri>import</ri> Control.Monad.Zip <ri>as</ri> Z\n" +
                "\n" +
                "<nc>{-\n" +
                " - Multiline comment\n" +
                " - {-\n" +
                " -  - Nested comment\n" +
                " -  -}\n" +
                " -}</nc>\n" +
                "\n" +
                "<ri>class</ri> Fooable <vi>a</vi> <ri>where</ri>\n" +
                "    <vi>foo</vi> <cs>::</cs> <vi>a</vi> --^ Haddock comment\n" +
                "           <vs>-></vs> String\n" +
                "\n" +
                "-- Line comment.\n" +
                "\n" +
                "<ri>instance</ri> MonadZip Maybe <ri>where</ri>\n" +
                "    <vi>mzip</vi> <vs>=</vs> <vi>liftM2</vi> <sp>(</sp><sp>,</sp><sp>)</sp>\n" +
                "\n" +
                "(<vs><~></vs>) <cs>::</cs> Maybe <vi>a</vi> <vs>-></vs> Maybe <vi>b</vi> <vs>-></vs> Maybe <sp>(</sp><vi>a</vi><sp>,</sp> <vi>b</vi><sp>)</sp>\n" +
                "(<vs><~></vs>) <vs>=</vs> <vi>mzip</vi>\n" +
                "\n" +
                "<vi>bar</vi> <cs>::</cs> [<vi>a</vi>] <vs>-></vs> Int <vs>-></vs> <sp>[</sp><vi>a</vi><sp>]</sp>\n" +
                "<vi>bar</vi> <vi>xs</vi> 0 <vs>=</vs> <sp>[</sp><sp>]</sp>\n" +
                "<vi>bar</vi> <vi>xs</vi> <vi>n</vi> <vs>=</vs> <vi>xs</vi> <vs>++</vs> <sp>(</sp><vi>bar</vi> <vi>xs</vi> <sp>(</sp><vi>n</vi> <vs>-</vs> 1<sp>)</sp><sp>)</sp>\n" +
                "\n" +
                "<vi>listToBool</vi> <cs>::</cs> <sp>[</sp><vi>a</vi><sp>]</sp> <vs>-></vs> Bool\n" +
                "<vi>listToBool</vi> <sp>[</sp><sp>]</sp> <vs>=</vs> False\n" +
                "<vi>listToBool</vi> <ri>_</ri> <vs>=</vs> True\n" +
                "\n" +
                "<vi>listToBool'</vi> <vi>xs</vi> <vs>=</vs> <ri>if</ri> <vi>length</vi> <vi>xs</vi> <vs>></vs> 0 <ri>then</ri> True <ri>else</ri> False\n" +
                "\n" +
                "<vi>listToBool''</vi> <vs>=</vs> <vi>not</vi> <vs>.</vs> <vi>null</vi>\n" +
                "\n" +
                "<vi>x</vi> <vs>=</vs> <sp>(</sp><vs>+</vs>1<sp>)</sp> <iv>`M.liftM`</iv> <sp>(</sp>Just 3<sp>)</sp>\n" +
                "\n" +
                "<vi>int</vi> <vs>=</vs> 1\n" +
                "<vi>float</vi> <vs>=</vs> 1.2\n" +
                "<vi>char</vi> <vs>=</vs> 'a'\n" +
                "<vi>string</vi> <vs>=</vs> \"I'm a string.\"\n" +
                "<vi>multiline</vi> <vs>=</vs> \"\\\n" +
                "    \\This string \\\n" +
                "    \\spans \\\"multiple\\\" \\\n" +
                "    \\lines!\\\n" +
                "    \\\"\n" +
                "quasiQuoted <vs>=</vs> <sp>[</sp>myQuoter|<qq>\n" +
                "   Here's some quasi quotes!\n" +
                "</qq><sp>|]</sp>";
    }

    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        @NonNls
        final Map<String, TextAttributesKey> map = new THashMap<String, TextAttributesKey>();
        map.put("ri", HaskellSyntaxHighlighter.RESERVEDID);
        map.put("vs", HaskellSyntaxHighlighter.VARSYM);
        map.put("cs", HaskellSyntaxHighlighter.CONSYM);
        map.put("vi", HaskellSyntaxHighlighter.VARID);
        map.put("iv", HaskellSyntaxHighlighter.INFIXVARID);
        map.put("nc", HaskellSyntaxHighlighter.NCOMMENT);
        map.put("sp", HaskellSyntaxHighlighter.SPECIAL);
        map.put("qq", HaskellSyntaxHighlighter.QQTEXT);
        return map;
    }

    @NotNull
    @Override
    public AttributesDescriptor[] getAttributeDescriptors() {
        return DESCRIPTORS;
    }

    @NotNull
    @Override
    public ColorDescriptor[] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @NotNull
    @Override
    public String getDisplayName() {
        return "Haskell";
    }
}
