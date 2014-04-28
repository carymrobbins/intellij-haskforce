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
VARIDREGEXP=[a-z_][a-zA-Z_0-9']*
CONID=[A-Z][a-zA-Z_0-9']*
CHARTOKEN='(\\.|[^'])'
INTEGERTOKEN=(0(o|O)[0-7]+|0(x|X)[0-9a-fA-F]+|[0-9]+)
FLOATTOKEN=([0-9]+\.[0-9]+((e|E)(\+|\-)?[0-9]+)?|[0-9]+((e|E)(\+|\-)?[0-9]+))
COMMENT=--([^\^\r\n][^\r\n]*|[\r\n])
DASHES=--(-)?
HADDOCK=--\^[^\r\n]*
STRINGTOKEN=\"(\\[ \t\n\x0B\f\r]*\n[ \t\n\x0B\f\r]*\\|\\\"|[^\"\n])*\"
CPPIF=#if ([^\r\n]*)

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
  "{-#"               { return OPENPRAGMA; }
  "#-}"               { return CLOSEPRAGMA; }
  "''"                { return THQUOTE; }
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
  "data"              { return DATA; }
  "default"           { return DEFAULT; }
  "deriving"          { return DERIVING; }
  "foreign"           { return FOREIGN; }
  "instance"          { return INSTANCE; }
  "module"            { return MODULE; }
  "newtype"           { return NEWTYPE; }
  "type"              { return TYPE; }
  "where"             { return WHERE; }
  "as"                { return AS; }
  "import"            { return IMPORT; }
  "infix"             { return INFIX; }
  "infixl"            { return INFIXL; }
  "infixr"            { return INFIXR; }
  "qualified"         { return QUALIFIED; }
  "hiding"            { return HIDING; }
  "case"              { return CASE; }
  "do"                { return DO; }
  "else"              { return ELSE; }
  "#else"             { return CPPELSE; }
  "#endif"            { return CPPENDIF; }
  "if"                { return IF; }
  "in"                { return IN; }
  "let"               { return LET; }
  "of"                { return OF; }
  "then"              { return THEN; }
  "LINE_WS"           { return LINE_WS; }
  "EOL"               { return EOL; }

  {DOUBLEQUOTE}       { return DOUBLEQUOTE; }
  {VARIDREGEXP}       { return VARIDREGEXP; }
  {CONID}             { return CONID; }
  {CHARTOKEN}         { return CHARTOKEN; }
  {INTEGERTOKEN}      { return INTEGERTOKEN; }
  {FLOATTOKEN}        { return FLOATTOKEN; }
  {COMMENT}           { return COMMENT; }
  {DASHES}            { return DASHES; }
  {HADDOCK}           { return HADDOCK; }
  {STRINGTOKEN}       { return STRINGTOKEN; }
  {CPPIF}             { return CPPIF; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
