package com.haskforce.cabal.psi;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static com.haskforce.cabal.psi.CabalTypes.*;

%%

%{
  public _CabalLexer() {
    this((java.io.Reader)null);
  }
%}

%public
%class _CabalLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode

EOL="\r"|"\n"|"\r\n"
LINE_WS=[\ \t\f]
WHITE_SPACE=({LINE_WS}|{EOL})+

COMMENT=--([^\^\r\n][^\r\n]*|[\r\n])
VARIDREGEXP=[a-zA-Z_0-9.']*
CRLF=([\r\n])

%%
<YYINITIAL> {
  {WHITE_SPACE}      { return com.intellij.psi.TokenType.WHITE_SPACE; }


  {COMMENT}          { return COMMENT; }
  {VARIDREGEXP}      { return VARIDREGEXP; }
  {CRLF}             { return CRLF; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
