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

ASCSMALL=[a-z]
ASCLARGE=[A-Z]
ASCDIGIT=[0-9]
HEXIT=[0-9a-fA-F]
OCTIT=[0-7]
OCTALPREFIX=0(o|O)
HEXADECIMALPREFIX=0(x|X)

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
  "--"                     { return DASHES; }
  "{-"                     { return OPENCOM; }
  "-}"                     { return CLOSECOM; }
  "\\""                    { return DOUBLEQUOTE; }
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
  "a"                      { return ACHARLOWER; }
  "b"                      { return BCHARLOWER; }
  "c"                      { return CCHARLOWER; }
  "d"                      { return DCHARLOWER; }
  "e"                      { return ECHARLOWER; }
  "f"                      { return FCHARLOWER; }
  "g"                      { return GCHARLOWER; }
  "h"                      { return HCHARLOWER; }
  "i"                      { return ICHARLOWER; }
  "j"                      { return JCHARLOWER; }
  "k"                      { return KCHARLOWER; }
  "l"                      { return LCHARLOWER; }
  "m"                      { return MCHARLOWER; }
  "n"                      { return NCHARLOWER; }
  "o"                      { return OCHARLOWER; }
  "p"                      { return PCHARLOWER; }
  "q"                      { return QCHARLOWER; }
  "r"                      { return RCHARLOWER; }
  "s"                      { return SCHARLOWER; }
  "t"                      { return TCHARLOWER; }
  "u"                      { return UCHARLOWER; }
  "v"                      { return VCHARLOWER; }
  "w"                      { return WCHARLOWER; }
  "x"                      { return XCHARLOWER; }
  "y"                      { return YCHARLOWER; }
  "z"                      { return ZCHARLOWER; }
  "A"                      { return ACHARUPPER; }
  "B"                      { return BCHARUPPER; }
  "C"                      { return CCHARUPPER; }
  "D"                      { return DCHARUPPER; }
  "E"                      { return ECHARUPPER; }
  "F"                      { return FCHARUPPER; }
  "G"                      { return GCHARUPPER; }
  "H"                      { return HCHARUPPER; }
  "I"                      { return ICHARUPPER; }
  "J"                      { return JCHARUPPER; }
  "K"                      { return KCHARUPPER; }
  "L"                      { return LCHARUPPER; }
  "M"                      { return MCHARUPPER; }
  "N"                      { return NCHARUPPER; }
  "O"                      { return OCHARUPPER; }
  "P"                      { return PCHARUPPER; }
  "Q"                      { return QCHARUPPER; }
  "R"                      { return RCHARUPPER; }
  "S"                      { return SCHARUPPER; }
  "T"                      { return TCHARUPPER; }
  "U"                      { return UCHARUPPER; }
  "V"                      { return VCHARUPPER; }
  "W"                      { return WCHARUPPER; }
  "X"                      { return XCHARUPPER; }
  "Y"                      { return YCHARUPPER; }
  "Z"                      { return ZCHARUPPER; }
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

  {ASCSMALL}               { return ASCSMALL; }
  {ASCLARGE}               { return ASCLARGE; }
  {ASCDIGIT}               { return ASCDIGIT; }
  {HEXIT}                  { return HEXIT; }
  {OCTIT}                  { return OCTIT; }
  {OCTALPREFIX}            { return OCTALPREFIX; }
  {HEXADECIMALPREFIX}      { return HEXADECIMALPREFIX; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
