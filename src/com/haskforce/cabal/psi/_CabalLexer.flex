package com.haskforce.cabal.psi;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static com.haskforce.cabal.psi.CabalTypes.*;

%%

%{
  private int indent;
  private Stack<Pair<Integer,Integer>> indentationStack;
  public _CabalLexer() {
    this((java.io.Reader)null);
    indentationStack = ContainerUtil.newStack();
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

%state REALLYYINITIAL

%%
<YYINITIAL> {
  [\ \f]          {
                     indent++;
                     return com.intellij.psi.TokenType.WHITE_SPACE;
                  }
  [\t]            {
                      indent = indent + (indent + 8) % 8;
                      return com.intellij.psi.TokenType.WHITE_SPACE;
                  }
  [^]             {
                      indentationStack.push(Pair.create(yyline, yycolumn));
                      yybegin(REALLYYINITIAL);
                      yypushback(1);
                      return WHITESPACELBRACETOK;
                  }
}

<REALLYYINITAL> {
  {WHITE_SPACE}      { return com.intellij.psi.TokenType.WHITE_SPACE; }
  ":"                { return COLON; }
  {COMMENT}          { return COMMENT; }
  {VARIDREGEXP}      { return VARIDREGEXP; }
  {CRLF}             { return CRLF; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
