package com.haskforce.system.settings;

import com.haskforce.system.ui.HaskellIcons;
import com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter;
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
            new AttributesDescriptor("Cons Symbol", HaskellSyntaxHighlighter.CONSYM),
            new AttributesDescriptor("Reserved Symbol", HaskellSyntaxHighlighter.RESERVEDOP),
            new AttributesDescriptor("Comma", HaskellSyntaxHighlighter.COMMA),
            new AttributesDescriptor("Semicolon", HaskellSyntaxHighlighter.SEMICOLON),
            new AttributesDescriptor("Brackets", HaskellSyntaxHighlighter.BRACKETS),
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
                "<ri>module</ri> Example <br>(</br><vi>foo</vi><co>,</co> <vi>bar</vi><br>)</br> <ri>where</ri>\n" +
                '\n' +
                "<ri>import</ri> Control.Monad <ri>as</ri> M\n" +
                "<ri>import</ri> Control.Monad <br>(</br><vi>liftM2</vi><br>)</br>\n" +
                "<ri>import</ri> Control.Monad.Zip <ri>as</ri> Z\n" +
                '\n' +
                "<nc>{-\n" +
                " - Multiline comment\n" +
                " - {-\n" +
                " -  - Nested comment\n" +
                " -  -}\n" +
                " -}</nc>\n" +
                '\n' +
                "<ri>class</ri> Fooable <vi>a</vi> <ri>where</ri>\n" +
                "    <vi>foo</vi> <ro>::</ro> <vi>a</vi> --^ Haddock comment\n" +
                "           <ro>-></ro> String\n" +
                '\n' +
                "-- Line comment.\n" +
                '\n' +
                "<ri>instance</ri> MonadZip Maybe <ri>where</ri>\n" +
                "    <vi>mzip</vi> <ro>=</ro> <vi>liftM2</vi> <br>(</br><co>,</co><br>)</br>\n" +
                '\n' +
                "(<vs><~></vs>) <ro>::</ro> Maybe <vi>a</vi> <ro>-></ro> Maybe <vi>b</vi> <ro>-></ro> Maybe <br>(</br><vi>a</vi><co>,</co> <vi>b</vi><br>)</br>\n" +
                "(<vs><~></vs>) <ro>=</ro> <vi>mzip</vi>\n" +
                '\n' +
                "<vi>bar</vi> <ro>::</ro> [<vi>a</vi>] <ro>-></ro> Int <ro>-></ro> <br>[</br><vi>a</vi><br>]</br>\n" +
                "<vi>bar</vi> <vi>xs</vi> 0 <ro>=</ro> <br>[</br><br>]</br>\n" +
                "<vi>bar</vi> <vi>xs</vi> <vi>n</vi> <ro>=</ro> <vi>xs</vi> <vs>++</vs> <br>(</br><vi>bar</vi> <vi>xs</vi> <br>(</br><vi>n</vi> <vs>-</vs> 1<br>)</br><br>)</br>\n" +
                '\n' +
                "<vi>listToBool</vi> <ro>::</ro> <br>[</br><vi>a</vi><br>]</br> <ro>-></ro> Bool\n" +
                "<vi>listToBool</vi> <br>[</br><br>]</br> <ro>=</ro> False\n" +
                "<vi>listToBool</vi> <ri>_</ri> <ro>=</ro> True\n" +
                '\n' +
                "<vi>listToBool'</vi> <vi>xs</vi> <ro>=</ro> <ri>if</ri> <vi>length</vi> <vi>xs</vi> <vs>></vs> 0 <ri>then</ri> True <ri>else</ri> False\n" +
                '\n' +
                "<vi>listToBool''</vi> <ro>=</ro> <vi>not</vi> <vs>.</vs> <vi>null</vi>\n" +
                '\n' +
                "<vi>x</vi> <ro>=</ro> <br>(</br><vs>+</vs>1<br>)</br> <iv>`M.liftM`</iv> <br>(</br>Just 3<br>)</br>\n" +
                '\n' +
                "<vi>int</vi> <ro>=</ro> 1\n" +
                "<vi>float</vi> <ro>=</ro> 1.2\n" +
                "<vi>char</vi> <ro>=</ro> 'a'\n" +
                "<vi>list</vi> <ro>=</ro> 1<cs>:</cs><br>[]</br>\n" +
                "<vi>string</vi> <ro>=</ro> \"I'm a string.\"\n" +
                "<vi>multiline</vi> <ro>=</ro> \"\\\n" +
                "    \\This string \\\n" +
                "    \\spans \\\"multiple\\\" \\\n" +
                "    \\lines!\\\n" +
                "    \\\"\n" +
                "quasiQuoted <ro>=</ro> <br>[</br>myQuoter|<qq>\n" +
                "   Here's some quasi quotes!\n" +
                "</qq><br>|]</br>";
    }

    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        @NonNls
        final Map<String, TextAttributesKey> map = new THashMap<String, TextAttributesKey>();
        map.put("ri", HaskellSyntaxHighlighter.RESERVEDID);
        map.put("ro", HaskellSyntaxHighlighter.RESERVEDOP);
        map.put("vs", HaskellSyntaxHighlighter.VARSYM);
        map.put("cs", HaskellSyntaxHighlighter.CONSYM);
        map.put("vi", HaskellSyntaxHighlighter.VARID);
        map.put("iv", HaskellSyntaxHighlighter.INFIXVARID);
        map.put("nc", HaskellSyntaxHighlighter.NCOMMENT);
        map.put("co", HaskellSyntaxHighlighter.COMMA);
        map.put("br", HaskellSyntaxHighlighter.BRACKETS);
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
