/**
 * Adapted from http://github.com/JetBrains/intellij-community
 * xml/xml-psi-impl/src/com/intellij/ide/highlighter/HtmlFileHighlighter.java
 */
package com.haskforce.yesod.shakespeare.hamlet.highlighting;

import com.haskforce.yesod.shakespeare.hamlet.psi.HamletTypes;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.XmlHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.xml.XmlTokenType;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class HamletSyntaxHighlighter extends SyntaxHighlighterBase {
    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new HamletSyntaxHighlightingLexer();
    }

    private static final Map<IElementType, TextAttributesKey> keys1;
    private static final Map<IElementType, TextAttributesKey> keys2;

    static {
        keys1 = new HashMap<IElementType, TextAttributesKey>(0);
        keys2 = new HashMap<IElementType, TextAttributesKey>(0);

        keys1.put(XmlTokenType.XML_COMMENT_START, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_COMMENT_END, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_COMMENT_CHARACTERS, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_CONDITIONAL_COMMENT_END, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_CONDITIONAL_COMMENT_END_START, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_CONDITIONAL_COMMENT_START, XmlHighlighterColors.HTML_COMMENT);
        keys1.put(XmlTokenType.XML_CONDITIONAL_COMMENT_START_END, XmlHighlighterColors.HTML_COMMENT);

        keys1.put(XmlTokenType.XML_START_TAG_START, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_END_TAG_START, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_TAG_END, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_EMPTY_ELEMENT_END, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_TAG_NAME, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.TAG_WHITE_SPACE, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_NAME, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_TAG_CHARACTERS, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_EQ, XmlHighlighterColors.HTML_TAG);

        keys2.put(XmlTokenType.XML_TAG_NAME, XmlHighlighterColors.HTML_TAG_NAME);
        // Not sure why, but seems this causes tag names to not highlight correctly.
        // keys2.put(XmlTokenType.XML_NAME, XmlHighlighterColors.HTML_ATTRIBUTE_NAME);
        keys2.put(XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN, XmlHighlighterColors.HTML_ATTRIBUTE_VALUE);
        keys2.put(XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER, XmlHighlighterColors.HTML_ATTRIBUTE_VALUE);
        keys2.put(XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER, XmlHighlighterColors.HTML_ATTRIBUTE_VALUE);
        keys2.put(XmlTokenType.XML_EQ, XmlHighlighterColors.HTML_ATTRIBUTE_NAME);

        keys1.put(XmlTokenType.XML_PI_START, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_PI_END, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_PI_TARGET, XmlHighlighterColors.HTML_TAG);
        keys2.put(XmlTokenType.XML_PI_TARGET, XmlHighlighterColors.HTML_TAG_NAME);

        keys1.put(XmlTokenType.XML_DOCTYPE_START, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_DOCTYPE_END, XmlHighlighterColors.HTML_TAG);
        keys1.put(XmlTokenType.XML_DOCTYPE_PUBLIC, XmlHighlighterColors.HTML_TAG);

        keys2.put(XmlTokenType.XML_CHAR_ENTITY_REF, XmlHighlighterColors.HTML_ENTITY_REFERENCE);
        keys2.put(XmlTokenType.XML_ENTITY_REF_TOKEN, XmlHighlighterColors.HTML_ENTITY_REFERENCE);

        keys1.put(XmlTokenType.XML_BAD_CHARACTER, HighlighterColors.BAD_CHARACTER);

        // Hamlet-specific highlights.
        keys1.put(HamletTypes.CLASS_ATTRIBUTE, XmlHighlighterColors.HTML_ATTRIBUTE_VALUE);
        keys1.put(HamletTypes.ID_ATTRIBUTE, XmlHighlighterColors.HTML_ATTRIBUTE_VALUE);
        keys1.put(HamletTypes.LOGIC, DefaultLanguageHighlighterColors.FUNCTION_CALL);
        keys1.put(HamletTypes.LINE_COMMENT, DefaultLanguageHighlighterColors.LINE_COMMENT);
        final IElementType[] interpolateElements = new IElementType[]{
                HamletTypes.HASKELL_INTERPOLATE_OPEN,
                HamletTypes.ROUTE_INTERPOLATE_OPEN,
                HamletTypes.WIDGET_INTERPOLATE_OPEN,
                HamletTypes.ATTR_INTERPOLATE_OPEN,
                HamletTypes.LANG_INTERPOLATE_OPEN,
                HamletTypes.INTERPOLATE_CLOSE,
        };
        for (IElementType elem : interpolateElements) {
            keys1.put(elem, DefaultLanguageHighlighterColors.INSTANCE_FIELD);
        }
        // TODO: It would be better to somehow highlight this as Haskell code...language injections?
        //keys1.put(HamletTypes.HASKELL_CODE, DefaultLanguageHighlighterColors.?);
    }

    @Override
    @NotNull
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(XmlHighlighterColors.HTML_CODE, pack(keys1.get(tokenType), keys2.get(tokenType)));
    }
}
