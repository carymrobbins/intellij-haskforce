package com.haskforce.cabal.psi;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static com.haskforce.cabal.psi.CabalTypes.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.openapi.util.Pair;
import com.intellij.util.containers.Stack;

%%

%{
  private int indent;
  private int yycolumn;
  private Stack<Integer> indentationStack;
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

%state FINDINDENTATIONCONTEXT, ININDENTATION

%%
<YYINITIAL> {
  [\ \f]             {
                        return com.intellij.psi.TokenType.WHITE_SPACE;
                     }
  [\t]               {
                         return com.intellij.psi.TokenType.WHITE_SPACE;
                     }
  "library"          {
                         yybegin(FINDINDENTATIONCONTEXT);
                         indent = yycolumn;
                         return LIBRARY;
                     }
  {WHITE_SPACE}      { return com.intellij.psi.TokenType.WHITE_SPACE; }
  ":"                { return COLON; }
  {COMMENT}          { return COMMENT; }
  {VARIDREGEXP}      { return VARIDREGEXP; }
}

<ININDENTATION> {
      {EOL}               {
                        yybegin(FINDINDENTATIONCONTEXT);
                        indent = 0;
                        return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\ \f]          {
                          indent++;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\t]            {
                          indent = indent + (indent + 8) % 8;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\n]            {
                          indent = 0;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      {WHITE_SPACE}   { return com.intellij.psi.TokenType.WHITE_SPACE; }
      ":"             { return COLON; }
      {VARIDREGEXP}   { return VARIDREGEXP; }
}

<FINDINDENTATIONCONTEXT> {
      [\ \f]          {
                          indent++;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\t]            {
                          indent = indent + (indent + 8) % 8;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\n]            {
                          indent = 0;
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [^]             {
                          yypushback(1);
                          if (indentationStack.isEmpty()){
                             indentationStack.push(yycolumn);
                             yybegin(ININDENTATION);
                             return WHITESPACELBRACETOK;
                          }  else {
                             if(indent == indentationStack.peek()){
                               yybegin(ININDENTATION);
                             } else {
                                  if(indent < indentationStack.peek()){
                                     yybegin(ININDENTATION);
                                     return WHITESPACERBRACETOK;
                                  } else {
                                     yybegin(ININDENTATION);
                                     return WHITESPACELBRACETOK;
                                  }
                             }
                          }
                      }

}


