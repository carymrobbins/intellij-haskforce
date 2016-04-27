package com.haskforce.cabal.lang.lexer;

import java.util.regex.*;

import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;

import com.haskforce.cabal.psi.CabalTypes;

%%

%{
  public _CabalParsingLexer() { this((java.io.Reader)null); }

  /** This should match the newline indent rule defined in our Flex lexer. */
  public static Pattern NEWLINE_INDENT_REGEX = Pattern.compile(
    "(\\r|\\n|\\r\\n)( *)", Pattern.MULTILINE
  );

  protected IElementType newlineIndentRule() {
    return newlineIndentRuleForValue(false);
  }

  protected IElementType newlineIndentRuleForValue() {
    return newlineIndentRuleForValue(true);
  }

  protected IElementType newlineIndentRuleForValue(boolean checkForValue) {
    final Matcher m = NEWLINE_INDENT_REGEX.matcher(yytext());
    if (!m.matches()) throw new AssertionError("NEWLINE_INDENT_REGEX did not match!");
    final String indent = m.group(2);
    if (checkForValue && indent.length() > indentLevel) {
      inValue = true;
      valueIndent = indentLevel;
    }
    //System.out.println("NIR: indent.length(): " + indent.length());
    //debugIndent();
    yypushback(indent.length());
    yybegin(INDENT);
    return CabalTypes.EOL;
  }

  protected void debugIndent() {
    System.out.println("NIR: indentLevel: " + indentLevel);
    System.out.println("NIR: inValue: " + inValue);
    System.out.println("NIR: valueIndent: " + valueIndent);
  }

  protected IElementType handleIndent() {
    //debugIndent();
    final int numWhitespace = yylength() - 1;

    if (currentLineIndent == 0) {
      if (numWhitespace == indentLevel) {
        // Consume all except the NON_WHITE_SPACE char
        yypushback(1);
        beginInitialOrValue();
        return CabalTypes.WHITE_SPACE;
      }
      if (numWhitespace > indentLevel) {
        // Consume up to the indentLevel + 1 for the INDENT
        currentLineIndent = indentLevel + 1;
        yypushback(yylength() - (indentLevel + 1));
        return CabalTypes.INDENT;
      }
      if (numWhitespace < indentLevel) {
        --indentLevel;
        yypushback(yylength());
        return CabalTypes.DEDENT;
      }
      // Shouldn't happen since we checked ==, >, and <
      throw new AssertionError(
        "Unexpected case: numWhitespace: " + numWhitespace + "; "
          + "indentLevel: " + indentLevel
      );
    }

    // If no more whitespace, start the line.
    if (numWhitespace == 0) {
      indentLevel = currentLineIndent;
      currentLineIndent = 0;
      yypushback(1);
      beginInitialOrValue();
      return CabalTypes.WHITE_SPACE;
      //return CabalTypes.LINE_START;
    }

    ++currentLineIndent;
    // Consume 1 whitespace char.
    yypushback(yylength() - 1);
    return CabalTypes.INDENT;
  }

  protected void beginInitialOrValue() {
    if (inValue) {
      if (indentLevel <= valueIndent) {
        inValue = false;
        yybegin(YYINITIAL);
      } else {
        yybegin(VALUE);
      }
    } else {
      yybegin(YYINITIAL);
    }
  }

  protected int currentLineIndent = 0;
  protected int indentLevel = 0;
  protected boolean inValue = false;
  protected int valueIndent = -1;
%}

%public
%class _CabalParsingLexer
%implements FlexLexer
%function advance
%type IElementType
%unicode
%ignorecase
%eof{ return;
%eof}

CRLF=\n|\r|\r\n
WHITE_SPACE=[\ ]
NOT_WHITE_SPACE=[^\ ]
KEY=[A-Za-z\-_][A-Za-z0-9\-_]*
DIGIT=[0-9]
COMMENT="--" [^\r\n]*
NEWLINE_INDENT={CRLF} {WHITE_SPACE}*

%state MAIN, VALUE, STANZA_ARGS, CONDITIONAL, INDENT

%%

// This entry point is the start of any line.  We always go to MAIN immediately
// after any of these rules, reason being that we need to know when we are
// at the start of a line so we know when to lex a comment.  The ony exceptions
// are when we lex a COMMENT or CRLF.
<YYINITIAL> {
  {WHITE_SPACE}* {COMMENT} { return CabalTypes.COMMENT; }
  {CRLF}*  { return CabalTypes.WHITE_SPACE; }
  [^]     { yypushback(yylength()); yybegin(MAIN); return CabalTypes.WHITE_SPACE; }
}

// Entry point after we've
<MAIN> {
  // TODO: How to avoid copy pasta with other lexical states?
  ":"   { return CabalTypes.COLON; }
  "{"   { return CabalTypes.LBRACE; }
  "}"   { return CabalTypes.RBRACE; }
  \t    { return CabalTypes.TAB; }

  // Keywords
  // TODO: How to avoid copy pasta with other lexical states?
  "if"      { yybegin(CONDITIONAL); return CabalTypes.IF; }
  "else"    { return CabalTypes.ELSE; }

  // Stanza names
  "library"           { yybegin(STANZA_ARGS); return CabalTypes.LIBRARY_KEY; }
  "executable"        { yybegin(STANZA_ARGS); return CabalTypes.EXECUTABLE_KEY; }
  "test-suite"        { yybegin(STANZA_ARGS); return CabalTypes.TEST_SUITE_KEY; }
  "benchmark"         { yybegin(STANZA_ARGS); return CabalTypes.BENCHMARK_KEY; }
  "source-repository" { yybegin(STANZA_ARGS); return CabalTypes.SOURCE_REPO_KEY; }
  "flag"              { yybegin(STANZA_ARGS); return CabalTypes.FLAG; }

  // Field names
  "name"                    { yybegin(VALUE); return CabalTypes.NAME_KEY; }
  "version"                 { yybegin(VALUE); return CabalTypes.VERSION_KEY; }
  "cabal-version"           { yybegin(VALUE); return CabalTypes.CABAL_VERSION_KEY; }
  "build-type"              { yybegin(VALUE); return CabalTypes.BUILD_TYPE_KEY; }
  "license"                 { yybegin(VALUE); return CabalTypes.LICENSE_KEY; }
  "license-file"            { yybegin(VALUE); return CabalTypes.LICENSE_FILE_KEY; }
  "license-files"           { yybegin(VALUE); return CabalTypes.LICENSE_FILES_KEY; }
  "copyright"               { yybegin(VALUE); return CabalTypes.COPYRIGHT_KEY; }
  "author"                  { yybegin(VALUE); return CabalTypes.AUTHOR_KEY; }
  "maintainer"              { yybegin(VALUE); return CabalTypes.MAINTAINER_KEY; }
  "stability"               { yybegin(VALUE); return CabalTypes.STABILITY_KEY; }
  "homepage"                { yybegin(VALUE); return CabalTypes.HOMEPAGE_KEY; }
  "bug-reports"             { yybegin(VALUE); return CabalTypes.BUG_REPORTS_KEY; }
  "package-url"             { yybegin(VALUE); return CabalTypes.PACKAGE_URL_KEY; }
  "synopsis"                { yybegin(VALUE); return CabalTypes.SYNOPSIS_KEY; }
  "description"             { yybegin(VALUE); return CabalTypes.DESCRIPTION_KEY; }
  "category"                { yybegin(VALUE); return CabalTypes.CATEGORY_KEY; }
  "tested-with"             { yybegin(VALUE); return CabalTypes.TESTED_WITH_KEY; }
  "data-files"              { yybegin(VALUE); return CabalTypes.DATA_FILES_KEY; }
  "data-dir"                { yybegin(VALUE); return CabalTypes.DATA_DIR_KEY; }
  "extra-source-files"      { yybegin(VALUE); return CabalTypes.EXTRA_SOURCE_FILES_KEY; }
  "extra-doc-files"         { yybegin(VALUE); return CabalTypes.EXTRA_DOC_FILES_KEY; }
  "extra-tmp-files"         { yybegin(VALUE); return CabalTypes.EXTRA_TMP_FILES_KEY; }
  "default"                 { yybegin(VALUE); return CabalTypes.DEFAULT_KEY; }
  "manual"                  { yybegin(VALUE); return CabalTypes.MANUAL_KEY; }
  "type"                    { yybegin(VALUE); return CabalTypes.TYPE_KEY; }
  "main-is"                 { yybegin(VALUE); return CabalTypes.MAIN_IS_KEY; }
  "module"                  { yybegin(VALUE); return CabalTypes.MODULE_KEY; }
  "location"                { yybegin(VALUE); return CabalTypes.LOCATION_KEY; }
  "branch"                  { yybegin(VALUE); return CabalTypes.BRANCH_KEY; }
  "tag"                     { yybegin(VALUE); return CabalTypes.TAG_KEY; }
  "subdir"                  { yybegin(VALUE); return CabalTypes.SUBDIR_KEY; }
  "build-depends"           { yybegin(VALUE); return CabalTypes.BUILD_DEPENDS_KEY; }
  "other-modules"           { yybegin(VALUE); return CabalTypes.OTHER_MODULES_KEY; }
  "default-language"        { yybegin(VALUE); return CabalTypes.DEFAULT_LANGUAGE_KEY; }
  "other-languages"         { yybegin(VALUE); return CabalTypes.OTHER_LANGUAGES_KEY; }
  "default-extensions"      { yybegin(VALUE); return CabalTypes.DEFAULT_EXTENSIONS_KEY; }
  "other-extensions"        { yybegin(VALUE); return CabalTypes.OTHER_EXTENSIONS_KEY; }
  "hs-source-dirs"          { yybegin(VALUE); return CabalTypes.HS_SOURCE_DIRS_KEY; }
  "extensions"              { yybegin(VALUE); return CabalTypes.EXTENSIONS_KEY; }
  "build-tools"             { yybegin(VALUE); return CabalTypes.BUILD_TOOLS_KEY; }
  "buildable"               { yybegin(VALUE); return CabalTypes.BUILDABLE_KEY; }
  "ghc-options"             { yybegin(VALUE); return CabalTypes.GHC_OPTIONS_KEY; }
  "ghc-prof-options"        { yybegin(VALUE); return CabalTypes.GHC_PROF_OPTIONS_KEY; }
  "ghc-shared-options"      { yybegin(VALUE); return CabalTypes.GHC_SHARED_OPTIONS_KEY; }
  "includes"                { yybegin(VALUE); return CabalTypes.INCLUDES_KEY; }
  "install-includes"        { yybegin(VALUE); return CabalTypes.INSTALL_INCLUDES_KEY; }
  "include-dirs"            { yybegin(VALUE); return CabalTypes.INCLUDE_DIRS_KEY; }
  "c-sources"               { yybegin(VALUE); return CabalTypes.C_SOURCES_KEY; }
  "js-sources"              { yybegin(VALUE); return CabalTypes.JS_SOURCES_KEY; }
  "extra-libraries"         { yybegin(VALUE); return CabalTypes.EXTRA_LIBRARIES_KEY; }
  "extra-ghci-libraries"    { yybegin(VALUE); return CabalTypes.EXTRA_GHCI_LIBRARIES_KEY; }
  "extra-lib-dirs"          { yybegin(VALUE); return CabalTypes.EXTRA_LIB_DIRS_KEY; }
  "cc-options"              { yybegin(VALUE); return CabalTypes.CC_OPTIONS_KEY; }
  "cpp-options"             { yybegin(VALUE); return CabalTypes.CPP_OPTIONS_KEY; }
  "ld-options"              { yybegin(VALUE); return CabalTypes.LD_OPTIONS_KEY; }
  "pkgconfig-depends"       { yybegin(VALUE); return CabalTypes.PKGCONFIG_DEPENDS_KEY; }
  "frameworks"              { yybegin(VALUE); return CabalTypes.FRAMEWORKS_KEY; }
  "exposed-modules"         { yybegin(VALUE); return CabalTypes.EXPOSED_MODULES_KEY; }
  "exposed"                 { yybegin(VALUE); return CabalTypes.EXPOSED_KEY; }
  "reexported-modules"      { yybegin(VALUE); return CabalTypes.REEXPORTED_MODULES_KEY; }
  "x-" {KEY}                { yybegin(VALUE); return CabalTypes.CUSTOM_KEY; }
  {KEY}                     { yybegin(VALUE); return CabalTypes.UNKNOWN_KEY; }

  {WHITE_SPACE}+  { return CabalTypes.WHITE_SPACE; }

  {NEWLINE_INDENT} { return newlineIndentRule(); }

  [^]   { return CabalTypes.OTHER_CHAR; }
}

<VALUE> {
  // TODO: How to avoid copy pasta with other lexical states?
  ":"   { return CabalTypes.COLON; }
  "("   { return CabalTypes.LPAREN; }
  ")"   { return CabalTypes.RPAREN; }
  "["   { return CabalTypes.LBRACKET; }
  "]"   { return CabalTypes.RBRACKET; }
  "{"   { return CabalTypes.LBRACE; }
  "}"   { return CabalTypes.RBRACE; }
  "=="  { return CabalTypes.EQ; }
  ">"   { return CabalTypes.GT; }
  ">="  { return CabalTypes.GTE; }
  "<"   { return CabalTypes.LT; }
  "<="  { return CabalTypes.LTE; }
  "&&"  { return CabalTypes.AND; }
  "||"  { return CabalTypes.OR; }
  "-"   { return CabalTypes.DASH; }
  "."   { return CabalTypes.DOT; }
  ","   { return CabalTypes.COMMA; }
  "!"   { return CabalTypes.BANG; }
  \t    { return CabalTypes.TAB; }

  // Keywords
  // TODO: How to avoid copy pasta with other lexical states?
  "with"    { return CabalTypes.WITH; }
  "true"    { return CabalTypes.TRUE; }
  "false"   { return CabalTypes.FALSE; }

  {DIGIT}+ ("." {DIGIT}+)* (".*"?)  { return CabalTypes.NUMBERS; }
  {KEY} { return CabalTypes.IDENT; }

  {NEWLINE_INDENT} { return newlineIndentRuleForValue(); }

  {WHITE_SPACE}+ { return CabalTypes.WHITE_SPACE; }

  [^]   { return CabalTypes.OTHER_CHAR; }
}

<STANZA_ARGS> {
  "{"   { return CabalTypes.LBRACE; }
  {WHITE_SPACE}+ { return CabalTypes.WHITE_SPACE; }
  {NEWLINE_INDENT} { return newlineIndentRule(); }
  {KEY} { return CabalTypes.IDENT; }
  [^] { return CabalTypes.OTHER_CHAR; }
}

<CONDITIONAL> {
  "flag"    { return CabalTypes.FLAG; }
  "os"      { return CabalTypes.OS; }
  "arch"    { return CabalTypes.ARCH; }
  "impl"    { return CabalTypes.IMPL; }
  "("       { return CabalTypes.LPAREN; }
  ")"       { return CabalTypes.RPAREN; }
  "!"       { return CabalTypes.BANG; }
  "{"       { return CabalTypes.LBRACE; }
  {KEY} { return CabalTypes.IDENT; }
  {WHITE_SPACE}+ { return CabalTypes.WHITE_SPACE; }
  {NEWLINE_INDENT} { return newlineIndentRule(); }
  [^]       { return CabalTypes.OTHER_CHAR; }
}

// This section is only entered after the newline indent rule.
<INDENT> {
  // Comments shouldn't affect the indentation.
  {WHITE_SPACE}* {COMMENT}  { return CabalTypes.COMMENT; }

  // A pure whitespace line can be disregarded.
  {WHITE_SPACE}* {CRLF} { currentLineIndent = 0; return CabalTypes.WHITE_SPACE; }

  // This rule only consumes zero or one whitespaces and returns an INDENT or DEDENT token.
  // The rule will be continually applied until there are zero whitespaces.
  {WHITE_SPACE}* {NOT_WHITE_SPACE} { return handleIndent(); }
}
