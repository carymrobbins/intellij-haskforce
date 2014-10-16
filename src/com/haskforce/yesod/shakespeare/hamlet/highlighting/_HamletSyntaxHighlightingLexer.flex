/**
 * Adapted from http://github.com/JetBrains/intellij-community
 * xml/xml-psi-impl/src/com/intellij/lexer/_HtmlLexer.flex
 */
package com.haskforce.yesod.shakespeare.hamlet.highlighting;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.*;
import com.intellij.psi.xml.*;
import com.haskforce.yesod.shakespeare.hamlet.psi.HamletTypes;

%%

%unicode

%{
  private IElementType elTokenType = XmlTokenType.XML_DATA_CHARACTERS;
  private IElementType elTokenType2 = XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN;

  public void setElTypes(IElementType _elTokenType,IElementType _elTokenType2) {
    elTokenType = _elTokenType;
    elTokenType2 = _elTokenType2;
  }

  public _HamletSyntaxHighlightingLexer() {
    this((java.io.Reader)null);
  }

  public final int yyIndexOf(char c) {
    int i = 0;
    for (; i < yylength(); ++i) {
        if (yycharat(i) == c) {
            return i;
        }
    }
    return -1;
  }

  // You must manually set this, it will not be updated for you.
  private int oldState = -1;

  public void handleInterpolation() {
    yypushback(yylength() - 2);
    oldState = yystate();
    yybegin(INTERPOLATION);
  }

  public static final Pattern interpolateOpenRegex = Pattern.compile("(#|@|\\^)\\{");
%}

%class _HamletSyntaxHighlightingLexer
%public
%implements FlexLexer
%function advance
%type IElementType
%eof{ return;
%eof}

%state DOC_TYPE
%state COMMENT
%state START_TAG_NAME
%state END_TAG_NAME
%state TAG_ATTRIBUTES
%state ATTRIBUTE_VALUE_START
%state ATTRIBUTE_VALUE_DQ
%state ATTRIBUTE_VALUE_SQ
%state PROCESSING_INSTRUCTION
%state START_TAG_NAME2
%state END_TAG_NAME2
%state TAG_CHARACTERS
%state C_COMMENT_START
%state C_COMMENT_END
%state LOGIC
%state INTERPOLATION
// 16 states! ^^
/* IMPORTANT! number of states should not exceed 16. See JspHighlightingLexer. */

ALPHA=[:letter:]
DIGIT=[0-9]
WHITE_SPACE_CHARS=[ \n\r\t\f]+

TAG_NAME=({ALPHA}|"_"|":")({ALPHA}|{DIGIT}|"_"|":"|"."|"-")*
TAG_NAME_FWT=("#")({ALPHA}|{DIGIT}|"_"|":"|"."|"-")*
ATTRIBUTE_NAME=({ALPHA}|"_"|":")({ALPHA}|{DIGIT}|"_"|":"|"."|"-")*

DTD_REF= "\"" [^\"]* "\"" | "'" [^']* "'"
DOCTYPE= "<!" (D|d)(O|o)(C|c)(T|t)(Y|y)(P|p)(E|e)
HTML= (H|h)(T|t)(M|m)(L|l)
PUBLIC= (P|p)(U|u)(B|b)(L|l)(I|i)(C|c)
EL_EMBEDDMENT="${" [^\}]* "}"

END_COMMENT="--"[ \n\r\t\f]*">"

CONDITIONAL_COMMENT_CONDITION=({ALPHA})({ALPHA}|{WHITE_SPACE_CHARS}|{DIGIT}|"."|"("|")"|"|"|"!"|"&")*

// Hamlet-specific patterns.
CLASS_ATTRIBUTE=(".")({ATTRIBUTE_NAME})
ID_ATTRIBUTE=("#")({ATTRIBUTE_NAME})
LOGIC=("$")("if"|"elseif"|"else"|"maybe"|"nothing"|"forall"|"case"|"of"|"with"|"doctype"|"newline")
LINE_COMMENT="$#" [^\r\n]*

%%

// Hamlet-specific rules.
"#{" .* { handleInterpolation(); return HamletTypes.HASKELL_INTERPOLATE_OPEN; }
"@{" .* { handleInterpolation(); return HamletTypes.ROUTE_INTERPOLATE_OPEN; }
"^{" .* { handleInterpolation(); return HamletTypes.WIDGET_INTERPOLATE_OPEN; }
"*{" .* { handleInterpolation(); return HamletTypes.ATTR_INTERPOLATE_OPEN; }
"_{" .* { handleInterpolation(); return HamletTypes.LANG_INTERPOLATE_OPEN; }

<INTERPOLATION> {
    [^\}]* { return HamletTypes.HASKELL_CODE; }
    "}" { yybegin(oldState); return HamletTypes.INTERPOLATE_CLOSE; }
}

<YYINITIAL> {LOGIC} {
    String text = yytext().toString();
    // Only drop into the LOGIC state if we require more params.
    if (!(text.equals("$else") || text.equals("$nothing"))) {
        yybegin(LOGIC);
    }
    return HamletTypes.LOGIC;
}

<LOGIC> [^\r\n]* { yybegin(YYINITIAL); return HamletTypes.HASKELL_CODE; }

<YYINITIAL> {LINE_COMMENT} { return HamletTypes.LINE_COMMENT; }

<TAG_ATTRIBUTES> {CLASS_ATTRIBUTE} { return HamletTypes.CLASS_ATTRIBUTE; }
<TAG_ATTRIBUTES> {ID_ATTRIBUTE} { return HamletTypes.ID_ATTRIBUTE; }
<TAG_ATTRIBUTES> {ATTRIBUTE_NAME} { return XmlTokenType.XML_NAME; }

// Original XML rules.

<YYINITIAL> "<?" { yybegin(PROCESSING_INSTRUCTION); return XmlTokenType.XML_PI_START; }
<PROCESSING_INSTRUCTION> "?"? ">" { yybegin(YYINITIAL); return XmlTokenType.XML_PI_END; }
<PROCESSING_INSTRUCTION> ([^\?\>] | (\?[^\>]))* { return XmlTokenType.XML_PI_TARGET; }

<YYINITIAL> {DOCTYPE} { yybegin(DOC_TYPE); return XmlTokenType.XML_DOCTYPE_START; }
<DOC_TYPE> {HTML} { return XmlTokenType.XML_NAME; }
<DOC_TYPE> {PUBLIC} { return XmlTokenType.XML_DOCTYPE_PUBLIC; }
<DOC_TYPE> {DTD_REF} { return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN;}
<DOC_TYPE> ">" { yybegin(YYINITIAL); return XmlTokenType.XML_DOCTYPE_END; }
<YYINITIAL> {WHITE_SPACE_CHARS} { return XmlTokenType.XML_REAL_WHITE_SPACE; }
<DOC_TYPE,TAG_ATTRIBUTES,ATTRIBUTE_VALUE_START,PROCESSING_INSTRUCTION, START_TAG_NAME, END_TAG_NAME, END_TAG_NAME2, TAG_CHARACTERS> {WHITE_SPACE_CHARS} { return XmlTokenType.XML_WHITE_SPACE; }
<YYINITIAL> "<" {TAG_NAME} { yybegin(START_TAG_NAME); yypushback(yylength()); }
<YYINITIAL> "<" {TAG_NAME_FWT} { yybegin(START_TAG_NAME2); yypushback(yylength()); }
<START_TAG_NAME, START_TAG_NAME2, TAG_CHARACTERS> "<" { return XmlTokenType.XML_START_TAG_START; }

<YYINITIAL> "</" {TAG_NAME} { yybegin(END_TAG_NAME); yypushback(yylength()); }
<YYINITIAL> "</" {TAG_NAME_FWT} { yybegin(END_TAG_NAME2); yypushback(yylength()); }
<YYINITIAL, END_TAG_NAME, END_TAG_NAME2> "</" { return XmlTokenType.XML_END_TAG_START; }

<YYINITIAL> "<!--" { yybegin(COMMENT); return XmlTokenType.XML_COMMENT_START; }
<COMMENT> "[" { yybegin(C_COMMENT_START); return XmlTokenType.XML_CONDITIONAL_COMMENT_START; }
<COMMENT> "<![" { yybegin(C_COMMENT_END); return XmlTokenType.XML_CONDITIONAL_COMMENT_END_START; }
<COMMENT> {END_COMMENT} { yybegin(YYINITIAL); return XmlTokenType.XML_COMMENT_END; }
<COMMENT> ">" {
  // according to HTML spec (http://www.w3.org/html/wg/drafts/html/master/syntax.html#comments)
  // comments should start with <!-- and end with --> thus making <!--> absolutely valid comment
  // please note that it's not true for XML (http://www.w3.org/TR/REC-xml/#sec-comments)
  int loc = getTokenStart();
  char prev = zzBuffer.charAt(loc - 1);
  char prevPrev = zzBuffer.charAt(loc - 2);
  if (prev == '-' && prevPrev == '-') {
    yybegin(YYINITIAL); return XmlTokenType.XML_COMMENT_END;
  }
  return XmlTokenType.XML_COMMENT_CHARACTERS;
}
<COMMENT> [^] { return XmlTokenType.XML_COMMENT_CHARACTERS; }

<C_COMMENT_START,C_COMMENT_END> {CONDITIONAL_COMMENT_CONDITION} { return XmlTokenType.XML_COMMENT_CHARACTERS; }
<C_COMMENT_START> [^] { yybegin(COMMENT); return XmlTokenType.XML_COMMENT_CHARACTERS; }
<C_COMMENT_START> "]>" { yybegin(COMMENT); return XmlTokenType.XML_CONDITIONAL_COMMENT_START_END; }
<C_COMMENT_START,C_COMMENT_END> {END_COMMENT} { yybegin(YYINITIAL); return XmlTokenType.XML_COMMENT_END; }
<C_COMMENT_END> "]" { yybegin(COMMENT); return XmlTokenType.XML_CONDITIONAL_COMMENT_END; }
<C_COMMENT_END> [^] { yybegin(COMMENT); return XmlTokenType.XML_COMMENT_CHARACTERS; }

<YYINITIAL> \\\$ {
  return XmlTokenType.XML_DATA_CHARACTERS;
}

<YYINITIAL> {EL_EMBEDDMENT} {
  return elTokenType;
}

<START_TAG_NAME, END_TAG_NAME> {TAG_NAME} { yybegin(TAG_ATTRIBUTES); return XmlTokenType.XML_NAME; }
<END_TAG_NAME2> {TAG_NAME_FWT} { return XmlTokenType.XML_NAME; }
<START_TAG_NAME2> {TAG_NAME_FWT} { yybegin(TAG_CHARACTERS); return XmlTokenType.XML_NAME; }

<TAG_ATTRIBUTES, END_TAG_NAME2, TAG_CHARACTERS> ">" { yybegin(YYINITIAL); return XmlTokenType.XML_TAG_END; }
<TAG_ATTRIBUTES, TAG_CHARACTERS> "/>" { yybegin(YYINITIAL); return XmlTokenType.XML_EMPTY_ELEMENT_END; }
<TAG_ATTRIBUTES> "=" { yybegin(ATTRIBUTE_VALUE_START); return XmlTokenType.XML_EQ; }
<TAG_ATTRIBUTES,START_TAG_NAME, END_TAG_NAME, END_TAG_NAME2> [^] { yybegin(YYINITIAL); yypushback(1); break; }

<TAG_CHARACTERS> [^] { return XmlTokenType.XML_TAG_CHARACTERS; }

<ATTRIBUTE_VALUE_START> ">" { yybegin(YYINITIAL); return XmlTokenType.XML_TAG_END; }
<ATTRIBUTE_VALUE_START> "/>" { yybegin(YYINITIAL); return XmlTokenType.XML_EMPTY_ELEMENT_END; }
 <ATTRIBUTE_VALUE_START> {EL_EMBEDDMENT} {
  return elTokenType2;
}

<ATTRIBUTE_VALUE_START> ([^ \n\r\t\f'\"\>]([^ \n\r\t\f\>]|(\/[^\>]))*) {
    // NOTE: Specialized Hamlet Hack.
    // We need to ensure that any interpolations inside of this match are captured
    // correctly.  If any exist, we need to look around to see if the attribute is
    // done and handle the state accordingly.
    Matcher m = interpolateOpenRegex.matcher(yytext());
    if (m.find()) {
        // Find the closing bracket position of the interpolation.
        int pos = yytext().toString().indexOf('}', m.start());
        // Push back the state so the attribute doesn't cover the interpolation.
        yypushback(yylength() - m.start());
        // If there is a whitespace after the interpolation, we are done with this attribute.
        if (pos != -1 && Character.isWhitespace(yycharat(pos + 1))) {
            yybegin(TAG_ATTRIBUTES);
        }
    } else {
        // If we didn't find an interpolation, we're done with this attribute.
        yybegin(TAG_ATTRIBUTES);
    }
    return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN;
}

<ATTRIBUTE_VALUE_START> "\"" { yybegin(ATTRIBUTE_VALUE_DQ); return XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER; }
<ATTRIBUTE_VALUE_START> "'" { yybegin(ATTRIBUTE_VALUE_SQ); return XmlTokenType.XML_ATTRIBUTE_VALUE_START_DELIMITER; }

// DQ == double quote
<ATTRIBUTE_VALUE_DQ> {
  "\"" { yybegin(TAG_ATTRIBUTES); return XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER; }
  \\\$ { return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN; }
  "${" [^\}\"]* "}" { return elTokenType2; }
  [^] { return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN;}
}

// SQ == single quote
<ATTRIBUTE_VALUE_SQ> {
  "'" { yybegin(TAG_ATTRIBUTES); return XmlTokenType.XML_ATTRIBUTE_VALUE_END_DELIMITER; }
  \\\$ { return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN; }
  "${" [^\}\']* "}" { return elTokenType2; }
  [^] { return XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN;}
}

"&lt;" |
"&gt;" |
"&apos;" |
"&quot;" |
"&nbsp;" |
"&amp;" |
"&#"{DIGIT}+";" |
"&#x"({DIGIT}|[a-fA-F])+";" { return XmlTokenType.XML_CHAR_ENTITY_REF; }
"&"{TAG_NAME}";" { return XmlTokenType.XML_ENTITY_REF_TOKEN; }

<YYINITIAL> ([^<&\$# \n\r\t\f]|(\\\$)|(\\#))* { return XmlTokenType.XML_DATA_CHARACTERS; }
<YYINITIAL> [^] { return XmlTokenType.XML_DATA_CHARACTERS; }
[^] { return XmlTokenType.XML_BAD_CHARACTER; }
