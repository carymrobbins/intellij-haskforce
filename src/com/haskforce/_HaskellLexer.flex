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

COMMENT=--[^\r\n]*
DASHES=--(-)*
DOUBLEQUOTE=\"
DIGIT=[0-9]
OCTALESCAPE=o[0-7][0-7]*
HEXADECIMALESCAPE=x[0-9a-fA-F][0-9a-fA-F]*
OCTALLITERAL=0(o|O)[0-7]+
HEXADECIMALLITERAL=0(x|X)[0-9a-fA-F]+
CHARESC=\\(a|b|f|n|r|t|v|\\|\"|\||'|&)
EXPONENTPREFIX=(e|E)(\+|\-)?
VARIDREGEXP=[a-z_][a-zA-Z_0-9']*
CONID=[A-Z][a-zA-Z_0-9']*
SYMBOL=[!#$%&*+./<=>?@'\^\|\-~:]
GRAPHIC=[a-zA-Z0-9\"'!#$%&*+./<=>?@'\^\|\-~:(),;\[\]`{}]
WHITEESCAPES=[\r\n\v\t]

%%
<YYINITIAL> {
  {WHITE_SPACE}             { return com.intellij.psi.TokenType.WHITE_SPACE; }

  "("                       { return LPAREN; }
  ")"                       { return RPAREN; }
  "|"                       { return PIPE; }
  ","                       { return COMMA; }
  ";"                       { return SEMICOLON; }
  "["                       { return LBRACKET; }
  "]"                       { return RBRACKET; }
  "`"                       { return BACKTICK; }
  "{"                       { return LBRACE; }
  "}"                       { return RBRACE; }
  " "                       { return SPACE; }
  "{-"                      { return OPENCOM; }
  "-}"                      { return CLOSECOM; }
  "'"                       { return SINGLEQUOTE; }
  "!"                       { return EXLAMATION; }
  "#"                       { return HASH; }
  "$"                       { return DOLLAR; }
  "%"                       { return PERCENT; }
  "&"                       { return AMPERSAND; }
  "*"                       { return ASTERISK; }
  "+"                       { return PLUS; }
  "."                       { return PERIOD; }
  "/"                       { return SLASH; }
  "<"                       { return LESSTHAN; }
  "="                       { return EQUALS; }
  ">"                       { return GREATERTHAN; }
  "?"                       { return QUESTION; }
  "@"                       { return AMPERSAT; }
  "\\"                      { return BACKSLASH; }
  "^"                       { return CARET; }
  "-"                       { return MINUS; }
  "~"                       { return TILDE; }
  ":"                       { return COLON; }
  ".."                      { return DOUBLEPERIOD; }
  "::"                      { return DOUBLECOLON; }
  "<-"                      { return LEFTARROW; }
  "->"                      { return RIGHTARROW; }
  "=>"                      { return DOUBLEARROW; }
  "\\&"                     { return NULLCHARACTER; }
  "class"                   { return CLASSTOKEN; }

  {COMMENT}                 { return COMMENT; }
  {DASHES}                  { return DASHES; }
  {DOUBLEQUOTE}             { return DOUBLEQUOTE; }
  {DIGIT}                   { return DIGIT; }
  {OCTALESCAPE}             { return OCTALESCAPE; }
  {HEXADECIMALESCAPE}       { return HEXADECIMALESCAPE; }
  {OCTALLITERAL}            { return OCTALLITERAL; }
  {HEXADECIMALLITERAL}      { return HEXADECIMALLITERAL; }
  {CHARESC}                 { return CHARESC; }
  {EXPONENTPREFIX}          { return EXPONENTPREFIX; }
  {VARIDREGEXP}             { return VARIDREGEXP; }
  {CONID}                   { return CONID; }
  {SYMBOL}                  { return SYMBOL; }
  {GRAPHIC}                 { return GRAPHIC; }
  {WHITEESCAPES}            { return WHITEESCAPES; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
