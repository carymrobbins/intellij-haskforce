package com.haskforce.cabal.psi;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static com.haskforce.cabal.psi.CabalTypes.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.openapi.util.Pair;
import com.intellij.util.containers.Stack;

%%

%{
  private int yycolumn;
  private Stack<Integer> indentationStack;
  private Stack<Integer> stateStack;
  public _CabalParsingLexer() {
    this((java.io.Reader)null);
    indentationStack = ContainerUtil.newStack();
    stateStack = ContainerUtil.newStack();
  }
%}

%public
%class _CabalParsingLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode
%column
%ignorecase


EOL="\r"|"\n"|"\r\n"
LINE_WS=[\ \t\f]
WHITE_SPACE=({LINE_WS}|{EOL})+

COMMENT=--([^\^\r\n][^\r\n]*|[\r\n])
VARIDREGEXP=[a-zA-Z_\-0-9']*
CONDITIONREGEXP=[a-zA-Z_\-0-9'()!.]*
FILEPATHREGEXP=[a-zA-Z_\-0-9'.*/]*
FREEFORMREGEXP=[^\r\n]*
NUMBERREGEXP=[0-9*]+
CRLF=([\r\n])

%state FINDINDENTATIONCONTEXT, CONFIGNAME, FREEFORM, FINDCOLON, VARID, FILEPATH, ININDENTATIONCONTEXT, CONDITION

%%
     <<EOF>>         {
                          if (! indentationStack.isEmpty ()){
                            indentationStack.pop();
                            return WHITESPACERBRACETOK;
                          }
                          return null;
                     }

<YYINITIAL> {

      [\ \f]          {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\t]            {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
    {EOL}           {
                         if (!indentationStack.isEmpty()){
                           yybegin(ININDENTATIONCONTEXT);
                         }
                         return com.intellij.psi.TokenType.WHITE_SPACE;
                    }
  "library"          {
                         yybegin(FINDINDENTATIONCONTEXT);
                         return LIBRARY;
                     }
  "if"               {
                        yybegin(CONDITION);
                        return IF;
                     }
  "else"             {
                        yybegin(FINDINDENTATIONCONTEXT);
                        return ELSE;
                     }
  "flag"             {
                         yybegin(CONFIGNAME);
                         return FLAG;
                     }
  "executable"       {
                           yybegin(CONFIGNAME);
                           return EXECUTABLE;
                     }
  "benchmark"       {
                           yybegin(CONFIGNAME);
                           return BENCHMARK;
                     }
  "source-repository" {
                           yybegin(CONFIGNAME);
                           return SOURCEREPOSITORYKEY;
                     }
  "test-suite"       {
                           yybegin(CONFIGNAME);
                           return TEST_SUITE;
                     }
  "type"             {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return TYPEKEY;
                     }
  "location"         {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return LOCATIONKEY;
                     }
   "module"             {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return MODULEKEY;
                     }
   "branch"             {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return BRANCHKEY;
                     }
   "tag"             {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return TAGKEY;
                     }
   "subdir"             {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return SUBDIRKEY;
                     }
  "test-module"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return TESTMODULEKEY;
                      }
  "main-is"      {
                     stateStack.push(FILEPATH);
                     yybegin(FINDCOLON);
                     return MAINISKEY;
                  }

  "name"          {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return NAMEKEY;
                    }
  "version"          {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return VERSIONKEY;
                    }
  "cabal-version"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return CABALVERSIONKEY;
                    }
  "synopsis"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return SYNOPSISKEY;
                    }
  "description"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return DESCRIPTIONKEY;
                    }
  "author"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return AUTHORKEY;
                    }
  "category"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return CATEGORYKEY;
                    }
  "build-type"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return BUILDTYPEKEY;
                    }
  "exposed"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return EXPOSEDKEY;
                    }
  "license"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return LICENSEKEY;
              }
  "license-file"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return LICENSEFILEKEY;
                    }
  "default"      {
                    stateStack.push(VARID);
                    yybegin(FINDCOLON);
                    return DEFAULTFLAGVALUEKEY;
                 }
  "manual"      {
                    stateStack.push(VARID);
                    yybegin(FINDCOLON);
                    return MANUALKEY;
                 }

  "license-files"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return LICENSEFILESKEY;
                    }
  "data-dir"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return DATADIRKEY;
                    }
  "stability"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return STABILITYKEY;
                    }
  "copyright"   {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return COPYRIGHTKEY;
                    }

  "data-files"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return DATAFILESKEY;
                    }

  "tested-with"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return TESTEDWITHKEY;
                    }
  "default-language"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return DEFAULTLANGUAGEKEY;
                    }
  "extra-source-files"   {
                       stateStack.push(FILEPATH);
                       yybegin(FINDCOLON);
                       return EXTRASOURCEFILESKEY;
                    }
  "extra-doc-files"   {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return EXTRADOCFILESKEY;
                    }

  "extra-tmp-files"   {
                       stateStack.push(VARID);

                       yybegin(FINDCOLON);
                       return EXTRATMPFILESKEY;
                    }
  "build-depends"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return BUILDDEPENDSKEY;
                       }
  "other-extensions"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return OTHEREXTENSIONSKEY;
                       }
  "other-modules"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return OTHERMODULESKEY;
                       }
  "exposed-modules"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return EXPOSEDMODULESKEY;
                       }
  "hs-source-dirs"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return HSSOURCEDIRSKEY;
                       }
  "extensions"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return EXTENSIONSKEY;
                       }
  "ghc-options"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return GHCOPTIONSKEY;
                       }
  "ghc-prof-options"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return GHCPROFOPTIONSKEY;
                       }
  "ghc-shared-options"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return GHCSHAREDOPTIONSKEY;
                       }
  "build-tools"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return BUILDTOOLSKEY;
                       }
  "includes"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return INCLUDESKEY;
                       }
  "includes-includes"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return INSTALLINCLUDESKEY;
                       }
  "includes-dirs"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return INCLUDEDIRSKEY;
                       }
  "c-sources"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return CSOURCESKEY;
                       }
  "js-sources"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return JSSOURCESKEY;
                       }
  "extra-libraries"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return EXTRALIBRARIESKEY;
                       }
  "extra-ghci-libraries"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return EXTRAGHCILIBRARIESKEY;
                       }
  "extra-lib-dirs"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return EXTRALIBDIRSKEY;
                       }
  "cc-options"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return CCOPTIONSKEY;
                       }
  "cpp-options"      {
                         stateStack.push(FREEFORM);
                         yybegin(FINDCOLON);
                         return CPPOPTIONSKEY;
                       }
  "ld-options"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return LDOPTIONSKEY;
                       }
  "pkg-config-depends"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return PKGCONFIGDEPENDSKEY;
                       }
  "frameworks"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return FRAMEWORKSKEY;
                       }
  "buildable"      {
                         stateStack.push(VARID);
                         yybegin(FINDCOLON);
                         return BUILDABLEKEY;
                       }
  "maintainer"    {
                    stateStack.push(FREEFORM);
                    yybegin(FINDCOLON);
                    return MAINTAINERKEY;
                  }
  "homepage"        {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return HOMEPAGEKEY;
                    }
  "bug-reports"      {
                       stateStack.push(FREEFORM);
                       yybegin(FINDCOLON);
                       return BUGREPORTSKEY;
                    }
  "package"         {
                       stateStack.push(VARID);
                       yybegin(FINDCOLON);
                       return PACKAGEKEY;
                    }

  {COMMENT}          { return COMMENT; }

  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<VARID> {
 [\ \f]          {
                     return com.intellij.psi.TokenType.WHITE_SPACE;
                 }
 [\t]            {
                     return com.intellij.psi.TokenType.WHITE_SPACE;
                 }
  "true" | "True" {
                       return TRUE;
                      }
  "false" | "False" {
                       return FALSE;
                      }
  ","             {
                       return COMMA;
                      }
  "."             {
                       return DOT;
                      }
  "/"             {
                       return SLASH;
                      }
  "=="            {
                    return EQ;
                  }
  "="            {
                    return ASSIGN;
                  }
  ">="            {
                    return GTEQ;
                  }
  "<="            {
                    return LTEQ;
                  }
  ">"            {
                    return GT;
                  }
  "<"            {
                    return LT;
                  }
  "&&"            {
                    return AND;
                  }

  {NUMBERREGEXP}     {
                    return NUMBERREGEXP;
                    }
  {VARIDREGEXP}      {
                     return VARIDREGEXP;
                     }
  {EOL}           {
                       if (!indentationStack.isEmpty()){
                         yybegin(ININDENTATIONCONTEXT);
                       }
                       return com.intellij.psi.TokenType.WHITE_SPACE;
                  }
  {COMMENT}          { return COMMENT; }
  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<FINDCOLON> {
  {LINE_WS}       {return com.intellij.psi.TokenType.WHITE_SPACE;}
  ":"             {
                      yybegin(FINDINDENTATIONCONTEXT);
                      return COLON;
                  }
  {COMMENT}          { return COMMENT; }
  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<CONDITION> {
 [\ ]            {return com.intellij.psi.TokenType.WHITE_SPACE;}
  {FREEFORMREGEXP}   {
                     yybegin(FINDINDENTATIONCONTEXT);
                     return FREEFORMREGEXP;
                  }
 {COMMENT}          { return COMMENT; }
 [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<CONFIGNAME> {
  [\ ]            {return com.intellij.psi.TokenType.WHITE_SPACE;}
  {VARIDREGEXP}   {
                     yybegin(FINDINDENTATIONCONTEXT);
                     return VARIDREGEXP;
                  }
  {COMMENT}          { return COMMENT; }
  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<FREEFORM> {
  [\ ]            {return com.intellij.psi.TokenType.WHITE_SPACE;}
  {FREEFORMREGEXP}     {
                     yybegin(YYINITIAL);
                     return FREEFORMREGEXP;
                  }
  {COMMENT}          { return COMMENT; }
  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<FILEPATH> {
  [\ ]            {return com.intellij.psi.TokenType.WHITE_SPACE;}
  {FILEPATHREGEXP}     {
                     yybegin(YYINITIAL);
                     return FILEPATHREGEXP;
                  }
  {COMMENT}          { return COMMENT; }
  [^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}

<ININDENTATIONCONTEXT> {
      [\ \f]          {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\t]            {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\n]            {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      {COMMENT}          { return COMMENT; }
      [^]             {
                          yypushback(1);

                          if(yycolumn == indentationStack.peek()){
                            yybegin(stateStack.isEmpty()?YYINITIAL:stateStack.peek());
                          } else {
                                if(yycolumn < indentationStack.peek()){
                                   indentationStack.pop();
                                   if(!stateStack.isEmpty()){
                                     stateStack.pop();
                                   }
                                   if (!indentationStack.isEmpty() && yycolumn == indentationStack.peek()){
                                      yybegin(stateStack.isEmpty()?YYINITIAL:stateStack.peek());

                                   }
                                   if (indentationStack.isEmpty()) {
                                      yybegin(YYINITIAL);
                                   }
                                   return WHITESPACERBRACETOK;
                                } else {
                                  yybegin(stateStack.isEmpty()?YYINITIAL:stateStack.peek());
                                }
                          }

                      }

}

<FINDINDENTATIONCONTEXT> {
      [\ \f]          {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\t]            {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      [\n]            {
                          return com.intellij.psi.TokenType.WHITE_SPACE;
                      }
      {COMMENT}          { return COMMENT; }
      [^]             {
                          yypushback(1);
                          if (indentationStack.isEmpty()){
                             if (yycolumn != 0) {
                               indentationStack.push(yycolumn);
                               yybegin(stateStack.isEmpty() ? YYINITIAL : stateStack.peek());
                               return WHITESPACELBRACETOK;
                             } else {
                               yybegin(YYINITIAL);
                             }
                          } else {
                               if(yycolumn == indentationStack.peek ()){
                                 if(!stateStack.isEmpty()){
                                   stateStack.pop();
                                 }
                                 yybegin(stateStack.isEmpty() ? YYINITIAL : stateStack.peek());
                                 return com.intellij.psi.TokenType.WHITE_SPACE;
                               } else {
                                 indentationStack.push(yycolumn);
                                 yybegin(stateStack.isEmpty() ? YYINITIAL : stateStack.peek());
                                 return WHITESPACELBRACETOK;
                               }

                          }
                      }

}


