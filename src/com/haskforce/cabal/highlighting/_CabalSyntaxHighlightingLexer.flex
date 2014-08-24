package com.haskforce.cabal.highlighting;

import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;
import com.haskforce.cabal.psi.CabalTypes;

%%

%{
  public _CabalSyntaxHighlightingLexer() {
    this((java.io.Reader)null);
  }
%}

%public
%class _CabalSyntaxHighlightingLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode
%ignorecase
%eof{ return;
%eof}

CRLF= \n|\r|\r\n
WHITE_SPACE=[\ \t\f]
KEY_PATTERN=[A-Za-z0-9\-]+\ *:
END_OF_LINE_COMMENT=--[^\r\n]*
COLON=:
CONFIG=(executable|library|benchmark|test-suite|flag )[^\n\r]*

%state WAITING_VALUE

%%

<YYINITIAL> {END_OF_LINE_COMMENT}                   { yybegin(YYINITIAL); return CabalTypes.COMMENT; }
<YYINITIAL> {COLON}                                 { yybegin(WAITING_VALUE); return CabalTypes.COLON; }
<YYINITIAL> {KEY_PATTERN}                           { yypushback(1); yybegin(YYINITIAL); return CabalTypes.KEY; }
<YYINITIAL> {CONFIG}                                { yybegin(YYINITIAL); return CabalTypes.CONFIG; }
<YYINITIAL> {WHITE_SPACE}+                          { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
<YYINITIAL> {CRLF}                                  { yybegin(YYINITIAL); return CabalTypes.CRLF; }
<WAITING_VALUE> {CRLF} {WHITE_SPACE}* ({KEY_PATTERN} | {CONFIG})
                                                    { yypushback(yylength()); yybegin(YYINITIAL); return CabalTypes.CRLF; }
<WAITING_VALUE> {END_OF_LINE_COMMENT}               { return CabalTypes.COMMENT; }
<WAITING_VALUE> [^]                                 { return CabalTypes.VALUE_CHAR; }
[^]                                                 { return TokenType.BAD_CHARACTER; }
