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
VARIDREGEXP=[a-zA-Z_\-0-9']*
NUMBERREGEXP=[0-9]+
CRLF=([\r\n])

%state FINDINDENTATIONCONTEXT, ININDENTATION, CONFIGNAME

%%
<YYINITIAL> {
    [\ \f]          {
                        return com.intellij.psi.TokenType.WHITE_SPACE;
                    }
    [\t]            {
                        return com.intellij.psi.TokenType.WHITE_SPACE;
                    }
  "library"          {
                         yybegin(FINDINDENTATIONCONTEXT);
                         indent = yycolumn;
                         return LIBRARY;
                     }
  "executable"       {
                           yybegin(CONFIGNAME);
                           return EXECUTABLE;
                     }
  "flag"             {
                           yybegin(CONFIGNAME);
                           return FLAG;
                     }
  {WHITE_SPACE}      { return com.intellij.psi.TokenType.WHITE_SPACE; }
  ":"                { return COLON; }
  ","                { return COMMA; }
  "."                { return DOT; }
  "=="            { return EQ;}
  ">="            { return GTEQ;}
  "<="            { return LTEQ;}
  ">"             { return GT;}
  "<"             { return LT;}
  "&&"            { return AND;}
  "name"          { return NAMEKEY; }
  "version"       { return VERSIONKEY; }
  "cabal-version" { return CABALVERSIONKEY; }
  "synopsis"      { return SYNOPSISKEY; }
  "author"        { return AUTHORKEY; }
  "maintainer"    { return MAINTAINERKEY; }
  "category"      { return CATEGORYKEY; }
  "build-type"      { return BUILDTYPEKEY; }
  "default-language"      { return DEFAULTLANGUAGEKEY; }
  "extra-source-files"      { return EXTRASOURCEFILESKEY; }
  "build-depends"      { return BUILDDEPENDSKEY; }
  "other-extensions"      { return OTHEREXTENSIONSKEY; }
  "other-modules"      { return OTHERMODULESKEY; }
  "exposed-modules"      { return EXPOSEDMODULESKEY; }
  {COMMENT}          { return COMMENT; }
  {NUMBERREGEXP}     { return NUMBERREGEXP; }

  {VARIDREGEXP}      { return VARIDREGEXP; }
}

<CONFIGNAME> {
  [\ ]            {return com.intellij.psi.TokenType.WHITE_SPACE;}
  {VARIDREGEXP}   {
                     yybegin(FINDINDENTATIONCONTEXT);
                     indent = yycolumn;
                     return VARIDREGEXP;
                  }
}

<ININDENTATION> {
     <<EOF>>          {
                          yybegin(YYINITIAL);
                          if (! indentationStack.isEmpty ()){
                             return WHITESPACERBRACETOK;
                          }

                      }
      {EOL}           {
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
      "=="            { return EQ;}
      ">="            { return GTEQ;}
      "<="            { return LTEQ;}
      ">"             { return GT;}
      "<"             { return LT;}
      "&&"            { return AND;}
      ":"             { return COLON; }
      "name"          { return NAMEKEY; }
      "version"       { return VERSIONKEY; }
      "cabal-version" { return CABALVERSIONKEY; }
      "synopsis"      { return SYNOPSISKEY; }
      "author"        { return AUTHORKEY; }
      "maintainer"    { return MAINTAINERKEY; }
      "category"      { return CATEGORYKEY; }
      "build-type"      { return BUILDTYPEKEY; }
      "default-language"      { return DEFAULTLANGUAGEKEY; }
      "extra-source-files"      { return EXTRASOURCEFILESKEY; }
      "build-depends"      { return BUILDDEPENDSKEY; }
      "other-extensions"      { return OTHEREXTENSIONSKEY; }
      "other-modules"      { return OTHERMODULESKEY; }
      "exposed-modules"      { return EXPOSEDMODULESKEY; }
      {NUMBERREGEXP}     { return NUMBERREGEXP; }
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
                             indentationStack.push(indent);
                             yybegin(ININDENTATION);
                             return WHITESPACELBRACETOK;
                          }  else {
                             if(indent == indentationStack.peek()){
                               yybegin(ININDENTATION);
                             } else {
                                   if(indent < indentationStack.peek()){
                                      indentationStack.pop();
                                      if (indentationStack.isEmpty()){
                                        yybegin(YYINITIAL);
                                      } else {
                                        yybegin(ININDENTATION);
                                      }
                                      return WHITESPACERBRACETOK;
                                   } else {
                                      yybegin(ININDENTATION);
                                      return WHITESPACELBRACETOK;
                                   }
                             }
                          }
                      }

}


