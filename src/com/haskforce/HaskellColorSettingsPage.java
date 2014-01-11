package com.haskforce;

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
            new AttributesDescriptor("Reserved Expression", HaskellSyntaxHighlighter.RESERVEDEXPR),
            new AttributesDescriptor("Reserved Declaration", HaskellSyntaxHighlighter.RESERVEDDECL),
            new AttributesDescriptor("Reserved Meta", HaskellSyntaxHighlighter.RESERVEDMETA),
            new AttributesDescriptor("Reserved Variable", HaskellSyntaxHighlighter.RESERVEDVAR),
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
            new AttributesDescriptor("Escape", HaskellSyntaxHighlighter.ESCAPE)
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
        return  "{-# LANGUAGE OverloadedStrings #-}\n" +
                "<rd>module</rd> Example <sp>(</sp><vi>foo</vi><sp>,</sp> <vi>bar</vi><sp>)</sp> <rd>where</rd>\n" +
                "\n" +
                "<rm>import</rm> Control.Monad <rm>as</rm> M\n" +
                "<rm>import</rm> Control.Monad <sp>(</sp><vi>liftM2</vi><sp>)</sp>\n" +
                "<rm>import</rm> Control.Monad.Zip <rm>as</rm> Z\n" +
                "\n" +
                "<nc>{-\n" +
                " - Multiline comment\n" +
                " - {-\n" +
                " -  - Nested comment\n" +
                " -  -}\n" +
                " -}</nc>\n" +
                "\n" +
                "<rd>class</rd> Fooable <vi>a</vi> <rd>where</rd>\n" +
                "    <vi>foo</vi> <ro>::</ro> <vi>a</vi> --^ Haddock comment\n" +
                "           <ro>-></ro> String\n" +
                "\n" +
                "-- Line comment.\n" +
                "\n" +
                "<rd>instance</rd> MonadZip Maybe <rd>where</rd>\n" +
                "    <vi>mzip</vi> <ro>=</ro> <vi>liftM2</vi> <sp>(</sp><sp>,</sp><sp>)</sp>\n" +
                "\n" +
                "(<vs><~></vs>) <ro>::</ro> Maybe <vi>a</vi> <ro>-></ro> Maybe <vi>b</vi> <ro>-></ro> Maybe <sp>(</sp><vi>a</vi><sp>,</sp> <vi>b</vi><sp>)</sp>\n" +
                "(<vs><~></vs>) <ro>=</ro> <vi>mzip</vi>\n" +
                "\n" +
                "<vi>bar</vi> <ro>::</ro> [<vi>a</vi>] <ro>-></ro> Int <ro>-></ro> <sp>[</sp><vi>a</vi><sp>]</sp>\n" +
                "<vi>bar</vi> <vi>xs</vi> 0 <ro>=</ro> <sp>[</sp><sp>]</sp>\n" +
                "<vi>bar</vi> <vi>xs</vi> <vi>n</vi> <ro>=</ro> <vi>xs</vi> <vs>++</vs> <sp>(</sp><vi>bar</vi> <vi>xs</vi> <sp>(</sp><vi>n</vi> <vs>-</vs> 1<sp>)</sp><sp>)</sp>\n" +
                "\n" +
                "<vi>listToBool</vi> <ro>::</ro> <sp>[</sp><vi>a</vi><sp>]</sp> <ro>-></ro> Bool\n" +
                "<vi>listToBool</vi> <sp>[</sp><sp>]</sp> <ro>=</ro> False\n" +
                "<vi>listToBool</vi> <rv>_</rv> <ro>=</ro> True\n" +
                "\n" +
                "<vi>listToBool'</vi> <vi>xs</vi> <ro>=</ro> <re>if</re> <vi>length</vi> <vi>xs</vi> <vs>></vs> 0 <re>then</re> True <re>else</re> False\n" +
                "\n" +
                "<vi>listToBool''</vi> <ro>=</ro> <vi>not</vi> <vs>.</vs> <vi>null</vi>\n" +
                "\n" +
                "<vi>x</vi> <ro>=</ro> <sp>(</sp><vs>+</vs>1<sp>)</sp> <iv>`M.liftM`</iv> <sp>(</sp>Just 3<sp>)</sp>\n" +
                "\n" +
                "<vi>int</vi> <ro>=</ro> 1\n" +
                "<vi>float</vi> <ro>=</ro> 1.2\n" +
                "<vi>char</vi> <ro>=</ro> 'a'\n" +
                "<vi>string</vi> <ro>=</ro> \"I'm a string.\"\n" +
                "<vi>multiline</vi> <ro>=</ro> \"\\\n" +
                "    \\This string \\\n" +
                "    \\spans \\\"multiple\\\" \\\n" +
                "    \\lines!\\\n" +
                "    \\\"\n";
    }

    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        @NonNls
        final Map<String, TextAttributesKey> map = new THashMap<String, TextAttributesKey>();
        map.put("re", HaskellSyntaxHighlighter.RESERVEDEXPR);
        map.put("rv", HaskellSyntaxHighlighter.RESERVEDVAR);
        map.put("rd", HaskellSyntaxHighlighter.RESERVEDDECL);
        map.put("rm", HaskellSyntaxHighlighter.RESERVEDMETA);
        map.put("vs", HaskellSyntaxHighlighter.VARSYM);
        map.put("vi", HaskellSyntaxHighlighter.VARID);
        map.put("iv", HaskellSyntaxHighlighter.INFIXVARID);
        map.put("nc", HaskellSyntaxHighlighter.NCOMMENT);
        map.put("ro", HaskellSyntaxHighlighter.RESERVEDOP);
        map.put("sp", HaskellSyntaxHighlighter.SPECIAL);
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
