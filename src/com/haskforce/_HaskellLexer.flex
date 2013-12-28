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

DASHES=--(-)*
ASCSMALL=[a-z]
ASCLARGE=[A-Z]
ASCDIGIT=[0-9]
HEXIT=[0-9a-fA-F]
OCTIT=[0-7]
OCTALESCAPE=o[0-7][0-7]*
HEXADECIMALESCAPE=x[0-9a-fA-F][0-9a-fA-F]*
OCTALPREFIX=0(o|O)
HEXADECIMALPREFIX=0(x|X)
CHARESC=(a|b|f|n|r|t|v|\|\"|'|&)

%%
<YYINITIAL> {
  {WHITE_SPACE}            { return com.intellij.psi.TokenType.WHITE_SPACE; }

  "("                      { return LPAREN; }
  ")"                      { return RPAREN; }
  "|"                      { return PIPE; }
  ","                      { return COMMA; }
  ";"                      { return SEMICOLON; }
  "["                      { return LBRACKET; }
  "]"                      { return RBRACKET; }
  "`"                      { return BACKTICK; }
  "{"                      { return LBRACE; }
  "}"                      { return RBRACE; }
  "\\n"                    { return RETURN; }
  "\\r"                    { return LINEFEED; }
  "\\v"                    { return VERTAB; }
  "\\f"                    { return FORMFEED; }
  " "                      { return SPACE; }
  "\\t"                    { return TAB; }
  "{-"                     { return OPENCOM; }
  "-}"                     { return CLOSECOM; }
  "\""                    { return DOUBLEQUOTE; }
  "'"                      { return SINGLEQUOTE; }
  "_"                      { return UNDERSCORE; }
  "!"                      { return EXLAMATION; }
  "#"                      { return HASH; }
  "$"                      { return DOLLAR; }
  "%"                      { return PERCENT; }
  "&"                      { return AMPERSAND; }
  "*"                      { return ASTERISK; }
  "+"                      { return PLUS; }
  "."                      { return PERIOD; }
  "/"                      { return SLASH; }
  "<"                      { return LESSTHAN; }
  "="                      { return EQUALS; }
  ">"                      { return GREATERTHAN; }
  "?"                      { return QUESTION; }
  "@"                      { return AMPERSAT; }
  "\\"                     { return BACKSLASH; }
  "^"                      { return CARET; }
  "-"                      { return MINUS; }
  "~"                      { return TILDE; }
  ":"                      { return COLON; }
  ".."                     { return DOUBLEPERIOD; }
  "::"                     { return DOUBLECOLON; }
  "<-"                     { return LEFTARROW; }
  "->"                     { return RIGHTARROW; }
  "=>"                     { return DOUBLEARROW; }
  "\\&"                    { return NULLCHARACTER; }
  "class"                  { return CLASSTOKEN; }
  "case"                   { return CASE; }
  "data"                   { return DATA; }
  "default"                { return DEFAULT; }
  "deriving"               { return DERIVING; }
  "do"                     { return DO; }
  "else"                   { return ELSE; }
  "foreign"                { return FOREIGN; }
  "if"                     { return IF; }
  "import"                 { return IMPORT; }
  "in"                     { return IN; }
  "infix"                  { return INFIX; }
  "infixl"                 { return INFIXL; }
  "infixr"                 { return INFIXR; }
  "instance"               { return INSTANCE; }
  "let"                    { return LET; }
  "module"                 { return MODULE; }
  "newtype"                { return NEWTYPE; }
  "of"                     { return OF; }
  "then"                   { return THEN; }
  "type"                   { return TYPE; }
  "where"                  { return WHERE; }
  "ascii"                  { return ASCII; }

  {DASHES}                 { return DASHES; }
  {ASCSMALL}               { return ASCSMALL; }
  {ASCLARGE}               { return ASCLARGE; }
  {ASCDIGIT}               { return ASCDIGIT; }
  {HEXIT}                  { return HEXIT; }
  {OCTIT}                  { return OCTIT; }
  {OCTALESCAPE}            { return OCTALESCAPE; }
  {HEXADECIMALESCAPE}      { return HEXADECIMALESCAPE; }
  {OCTALPREFIX}            { return OCTALPREFIX; }
  {HEXADECIMALPREFIX}      { return HEXADECIMALPREFIX; }
  {CHARESC}                { return CHARESC; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
