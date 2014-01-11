package com.haskforce;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static com.haskforce.psi.HaskellTypes.*;

%%

%{
  public _HaskellLexer() {
    this((java.io.Reader)null);
  }
%}

%public
%class _HaskellLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode

EOL="\r"|"\n"|"\r\n"
LINE_WS=[\ \t\f]
WHITE_SPACE=({LINE_WS}|{EOL})+

DOUBLEQUOTE=\"
CHARESC=\\(a|b|f|n|r|t|v|\\|\||'|&)
VARIDREGEXP=[a-z_][a-zA-Z_0-9']*
CONID=[A-Z][a-zA-Z_0-9']*
WHITEESCAPES=[\r\n\v\t]
STRINGTOKEN=\"(\\[ tnx0Bfr]+\\|\\\"|[^\"])*\"
CHARTOKEN='(\\.|[^'])'
INTEGERTOKEN=(0(o|O)[0-7]+|0(x|X)[0-9a-fA-F]+|[0-9]+)
FLOATTOKEN=([0-9]+\.[0-9]+((e|E)(\+|\-)?[0-9]+)?|[0-9]+((e|E)(\+|\-)?[0-9]+))
COMMENT=--[^\^\r\n][^\r\n]*
HADDOCK=--\^[^\r\n]*
DASHES=--(-)*
PRAGMA=\{\-\#[^\#]+\#\-\}

%%
<YYINITIAL> {
  {WHITE_SPACE}       { return com.intellij.psi.TokenType.WHITE_SPACE; }

  "("                 { return LPAREN; }
  ")"                 { return RPAREN; }
  "|"                 { return PIPE; }
  ","                 { return COMMA; }
  ";"                 { return SEMICOLON; }
  "["                 { return LBRACKET; }
  "]"                 { return RBRACKET; }
  "`"                 { return BACKTICK; }
  "{"                 { return LBRACE; }
  "}"                 { return RBRACE; }
  "{-"                { return OPENCOM; }
  "-}"                { return CLOSECOM; }
  " "                 { return SPACE; }
  "'"                 { return SINGLEQUOTE; }
  "!"                 { return EXLAMATION; }
  "#"                 { return HASH; }
  "$"                 { return DOLLAR; }
  "%"                 { return PERCENT; }
  "&"                 { return AMPERSAND; }
  "*"                 { return ASTERISK; }
  "+"                 { return PLUS; }
  "."                 { return PERIOD; }
  "/"                 { return SLASH; }
  "<"                 { return LESSTHAN; }
  "="                 { return EQUALS; }
  ">"                 { return GREATERTHAN; }
  "?"                 { return QUESTION; }
  "@"                 { return AMPERSAT; }
  "\\"                { return BACKSLASH; }
  "^"                 { return CARET; }
  "-"                 { return MINUS; }
  "~"                 { return TILDE; }
  ":"                 { return COLON; }
  ".."                { return DOUBLEPERIOD; }
  "::"                { return DOUBLECOLON; }
  "<-"                { return LEFTARROW; }
  "->"                { return RIGHTARROW; }
  "=>"                { return DOUBLEARROW; }
  "\\&"               { return NULLCHARACTER; }
  "class"             { return CLASSTOKEN; }

  {DOUBLEQUOTE}       { return DOUBLEQUOTE; }
  {CHARESC}           { return CHARESC; }
  {VARIDREGEXP}       { return VARIDREGEXP; }
  {CONID}             { return CONID; }
  {WHITEESCAPES}      { return WHITEESCAPES; }
  {STRINGTOKEN}       { return STRINGTOKEN; }
  {CHARTOKEN}         { return CHARTOKEN; }
  {INTEGERTOKEN}      { return INTEGERTOKEN; }
  {FLOATTOKEN}        { return FLOATTOKEN; }
  {COMMENT}           { return COMMENT; }
  {HADDOCK}           { return HADDOCK; }
  {DASHES}            { return DASHES; }
  {PRAGMA}            { return PRAGMA; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
