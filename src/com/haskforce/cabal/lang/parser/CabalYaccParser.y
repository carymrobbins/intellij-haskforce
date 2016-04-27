%{
  import com.intellij.lang.PsiBuilder;
  import com.intellij.psi.tree.IElementType;

  import com.haskforce.cabal.psi.CabalTokenType;
  import com.haskforce.cabal.psi.CabalTypes;
%}

%token AND ARCH AUTHOR_KEY BANG BENCHMARK_KEY BRANCH_KEY BUG_REPORTS_KEY BUILDABLE_KEY
%token BUILD_DEPENDS_KEY BUILD_TOOLS_KEY BUILD_TYPE_KEY CABAL_VERSION_KEY CATEGORY_KEY
%token CC_OPTIONS_KEY COLON COMMA COMMENT COPYRIGHT_KEY CPP_OPTIONS_KEY C_SOURCES_KEY
%token CUSTOM_KEY DASH DATA_DIR_KEY DATA_FILES_KEY DEDENT DEFAULT_EXTENSIONS_KEY
%token DEFAULT_KEY DEFAULT_LANGUAGE_KEY DESCRIPTION_KEY DOT ELSE EOL EQ EXECUTABLE_KEY
%token EXPOSED_KEY EXPOSED_MODULES_KEY EXTENSIONS_KEY EXTRA_DOC_FILES_KEY
%token EXTRA_GHCI_LIBRARIES_KEY EXTRA_LIB_DIRS_KEY EXTRA_LIBRARIES_KEY
%token EXTRA_SOURCE_FILES_KEY EXTRA_TMP_FILES_KEY FALSE FLAG FRAMEWORKS_KEY
%token GHC_OPTIONS_KEY GHC_PROF_OPTIONS_KEY GHC_SHARED_OPTIONS_KEY GT GTE HOMEPAGE_KEY
%token HS_SOURCE_DIRS_KEY IDENT IF IMPL INCLUDE_DIRS_KEY INCLUDES_KEY INDENT
%token INSTALL_INCLUDES_KEY JS_SOURCES_KEY LBRACE LBRACKET LD_OPTIONS_KEY LIBRARY_KEY
%token LICENSE_FILE_KEY LICENSE_FILES_KEY LICENSE_KEY LINE_START LOCATION_KEY LPAREN
%token LT LTE MAIN_IS_KEY MAINTAINER_KEY MANUAL_KEY MODULE_KEY NAME_KEY NUMBERS OR OS OTHER_CHAR
%token OTHER_EXTENSIONS_KEY OTHER_LANGUAGES_KEY OTHER_MODULES_KEY PACKAGE_URL_KEY
%token PKGCONFIG_DEPENDS_KEY RBRACE RBRACKET REEXPORTED_MODULES_KEY RPAREN
%token SOURCE_REPO_KEY STABILITY_KEY SUBDIR_KEY SYNOPSIS_KEY TAB TAG_KEY
%token TESTED_WITH_KEY TEST_SUITE_KEY TRUE TYPE_KEY UNKNOWN_KEY VERSION_KEY WITH

%%

  fieldOrStanzas
    : fieldOrStanza
    | fieldOrStanzas EOL fieldOrStanza
    | error { errorWith("Expected field or stanza", $1); }
    ;

  fieldOrStanza
    : field
    | stanza
    ;

  freeform
    : freeformBlock { $$ = markWith(CabalTypes.FREEFORM, $1); }
    | freeform freeformBlock { $$ = markWith(CabalTypes.FREEFORM, $1, $2); }
    ;

  freeformBlock
    : freeformLine
    | freeformBlock EOL INDENT freeformBlock DEDENT
    | freeformBlock INDENT freeformBlock DEDENT
    | freeformBlock LBRACE freeformBlock RBRACE
    ;

  freeformLine
    : freeformToks
    | freeformLine freeformToks
    ;

  freeformToks
    : freeformTok
    | freeformToks freeformTok
    ;

  freeformTok
    : COLON | LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE
    | EQ | GT | GTE | LT | LTE | AND | OR | DASH | DOT | COMMA | BANG
    | TAB | WITH | TRUE | FALSE | NUMBERS | IDENT
    | NUMBERS
    ;

  stanza
    : FLAG IDENT stanzaBody { $$ = markWith(CabalTypes.FLAG_DECL, $1, $3); }
    | LIBRARY_KEY stanzaBody { $$ = markWith(CabalTypes.LIBRARY, $1, $2); }
    | EXECUTABLE_KEY IDENT stanzaBody { $$ = markWith(CabalTypes.EXECUTABLE, $1, $3); }
    | TEST_SUITE_KEY IDENT stanzaBody { $$ = markWith(CabalTypes.TEST_SUITE, $1, $3); }
    | BENCHMARK_KEY IDENT stanzaBody { $$ = markWith(CabalTypes.BENCHMARK, $1, $3); }
    | SOURCE_REPO_KEY IDENT stanzaBody { $$ = markWith(CabalTypes.SOURCE_REPO, $1, $3); }
    ;

  stanzaBody
    : fields
    | EOL INDENT stanzaBody DEDENT
    | INDENT stanzaBody DEDENT
    | LBRACE stanzaBody RBRACE
    ;

  fields
    : field
    | fields EOL field
    ;

  field
    : AUTHOR_KEY COLON freeform { $$ = markWith(CabalTypes.AUTHOR, $1, $3); }
    | BENCHMARK_KEY COLON freeform { $$ = markWith(CabalTypes.BENCHMARK, $1, $3); }
    | BRANCH_KEY COLON freeform { $$ = markWith(CabalTypes.BRANCH, $1, $3); }
    | BUG_REPORTS_KEY COLON freeform { $$ = markWith(CabalTypes.BUG_REPORTS, $1, $3); }
    | BUILDABLE_KEY COLON freeform { $$ = markWith(CabalTypes.BUILDABLE, $1, $3); }
    | BUILD_DEPENDS_KEY COLON freeform { $$ = markWith(CabalTypes.BUILD_DEPENDS, $1, $3); }
    | BUILD_TOOLS_KEY COLON freeform { $$ = markWith(CabalTypes.BUILD_TOOLS, $1, $3); }
    | BUILD_TYPE_KEY COLON freeform { $$ = markWith(CabalTypes.BUILD_TYPE, $1, $3); }
    | CABAL_VERSION_KEY COLON freeform { $$ = markWith(CabalTypes.CABAL_VERSION, $1, $3); }
    | CATEGORY_KEY COLON freeform { $$ = markWith(CabalTypes.CATEGORY, $1, $3); }
    | CC_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.CC_OPTIONS, $1, $3); }
    | COPYRIGHT_KEY COLON freeform { $$ = markWith(CabalTypes.COPYRIGHT, $1, $3); }
    | CPP_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.CPP_OPTIONS, $1, $3); }
    | C_SOURCES_KEY COLON freeform { $$ = markWith(CabalTypes.C_SOURCES, $1, $3); }
    | CUSTOM_KEY COLON freeform { $$ = markWith(CabalTypes.CUSTOM_FIELD, $1, $3); }
    | DATA_DIR_KEY COLON freeform { $$ = markWith(CabalTypes.DATA_DIR, $1, $3); }
    | DATA_FILES_KEY COLON freeform { $$ = markWith(CabalTypes.DATA_FILES, $1, $3); }
    | DEFAULT_EXTENSIONS_KEY COLON freeform { $$ = markWith(CabalTypes.DEFAULT_EXTENSIONS, $1, $3); }
    | DEFAULT_KEY COLON freeform { $$ = markWith(CabalTypes.DEFAULT, $1, $3); }
    | DEFAULT_LANGUAGE_KEY COLON freeform { $$ = markWith(CabalTypes.DEFAULT_LANGUAGE, $1, $3); }
    | DESCRIPTION_KEY COLON freeform { $$ = markWith(CabalTypes.DESCRIPTION, $1, $3); }
    | EXECUTABLE_KEY COLON freeform { $$ = markWith(CabalTypes.EXECUTABLE, $1, $3); }
    | EXPOSED_KEY COLON freeform { $$ = markWith(CabalTypes.EXPOSED, $1, $3); }
    | EXPOSED_MODULES_KEY COLON freeform { $$ = markWith(CabalTypes.EXPOSED_MODULES, $1, $3); }
    | EXTENSIONS_KEY COLON freeform { $$ = markWith(CabalTypes.EXTENSIONS, $1, $3); }
    | EXTRA_DOC_FILES_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_DOC_FILES, $1, $3); }
    | EXTRA_GHCI_LIBRARIES_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_GHCI_LIBRARIES, $1, $3); }
    | EXTRA_LIB_DIRS_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_LIB_DIRS, $1, $3); }
    | EXTRA_LIBRARIES_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_LIBRARIES, $1, $3); }
    | EXTRA_SOURCE_FILES_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_SOURCE_FILES, $1, $3); }
    | EXTRA_TMP_FILES_KEY COLON freeform { $$ = markWith(CabalTypes.EXTRA_TMP_FILES, $1, $3); }
    | FRAMEWORKS_KEY COLON freeform { $$ = markWith(CabalTypes.FRAMEWORKS, $1, $3); }
    | GHC_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.GHC_OPTIONS, $1, $3); }
    | GHC_PROF_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.GHC_PROF_OPTIONS, $1, $3); }
    | GHC_SHARED_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.GHC_SHARED_OPTIONS, $1, $3); }
    | HOMEPAGE_KEY COLON freeform { $$ = markWith(CabalTypes.HOMEPAGE, $1, $3); }
    | HS_SOURCE_DIRS_KEY COLON freeform { $$ = markWith(CabalTypes.HS_SOURCE_DIRS, $1, $3); }
    | INCLUDE_DIRS_KEY COLON freeform { $$ = markWith(CabalTypes.INCLUDE_DIRS, $1, $3); }
    | INCLUDES_KEY COLON freeform { $$ = markWith(CabalTypes.INCLUDES, $1, $3); }
    | INSTALL_INCLUDES_KEY COLON freeform { $$ = markWith(CabalTypes.INSTALL_INCLUDES, $1, $3); }
    | JS_SOURCES_KEY COLON freeform { $$ = markWith(CabalTypes.JS_SOURCES, $1, $3); }
    | LD_OPTIONS_KEY COLON freeform { $$ = markWith(CabalTypes.LD_OPTIONS, $1, $3); }
    | LIBRARY_KEY COLON freeform { $$ = markWith(CabalTypes.LIBRARY, $1, $3); }
    | LICENSE_FILE_KEY COLON freeform { $$ = markWith(CabalTypes.LICENSE_FILE, $1, $3); }
    | LICENSE_FILES_KEY COLON freeform { $$ = markWith(CabalTypes.LICENSE_FILES, $1, $3); }
    | LICENSE_KEY COLON freeform { $$ = markWith(CabalTypes.LICENSE, $1, $3); }
    | LOCATION_KEY COLON freeform { $$ = markWith(CabalTypes.LOCATION, $1, $3); }
    | MAIN_IS_KEY COLON freeform { $$ = markWith(CabalTypes.MAIN_IS, $1, $3); }
    | MAINTAINER_KEY COLON freeform { $$ = markWith(CabalTypes.MAINTAINER, $1, $3); }
    | MANUAL_KEY COLON freeform { $$ = markWith(CabalTypes.MANUAL, $1, $3); }
    | NAME_KEY COLON freeform { $$ = markWith(CabalTypes.NAME, $1, $3); }
    | OTHER_EXTENSIONS_KEY COLON freeform { $$ = markWith(CabalTypes.OTHER_EXTENSIONS, $1, $3); }
    | OTHER_LANGUAGES_KEY COLON freeform { $$ = markWith(CabalTypes.OTHER_LANGUAGES, $1, $3); }
    | OTHER_MODULES_KEY COLON freeform { $$ = markWith(CabalTypes.OTHER_MODULES, $1, $3); }
    | PACKAGE_URL_KEY COLON freeform { $$ = markWith(CabalTypes.PACKAGE_URL, $1, $3); }
    | PKGCONFIG_DEPENDS_KEY COLON freeform { $$ = markWith(CabalTypes.PKGCONFIG_DEPENDS, $1, $3); }
    | REEXPORTED_MODULES_KEY COLON freeform { $$ = markWith(CabalTypes.REEXPORTED_MODULES, $1, $3); }
    | SOURCE_REPO_KEY COLON freeform { $$ = markWith(CabalTypes.SOURCE_REPO, $1, $3); }
    | STABILITY_KEY COLON freeform { $$ = markWith(CabalTypes.STABILITY, $1, $3); }
    | SUBDIR_KEY COLON freeform { $$ = markWith(CabalTypes.SUBDIR, $1, $3); }
    | SYNOPSIS_KEY COLON freeform { $$ = markWith(CabalTypes.SYNOPSIS, $1, $3); }
    | TAG_KEY COLON freeform { $$ = markWith(CabalTypes.TAG, $1, $3); }
    | TESTED_WITH_KEY COLON freeform { $$ = markWith(CabalTypes.TESTED_WITH, $1, $3); }
    | TEST_SUITE_KEY COLON freeform { $$ = markWith(CabalTypes.TEST_SUITE, $1, $3); }
    | TYPE_KEY COLON freeform { $$ = markWith(CabalTypes.TYPE, $1, $3); }
    | UNKNOWN_KEY COLON freeform { $$ = markWith(CabalTypes.UNKNOWN_FIELD, $1, $3); }
    | VERSION_KEY COLON freeform { $$ = markWith(CabalTypes.VERSION, $1, $3); }
    ;
%%

CabalYaccParser(PsiBuilder b) {
  this.b = b;
}

final PsiBuilder b;

void yyerror(String msg) {
  System.out.println("Parse error: " + msg);
}

int yylex() {
  if (b.eof()) return 0;
  IElementType typ = b.getTokenType();
  System.out.println(" --> " + typ);
  short v = getYYVal(typ);
  if (v < 1) throw new AssertionError("Invalid yyval for " + typ + ": " + v);
  PsiBuilder.Marker m = b.mark();
  yylval = newParserVal(m, typ, b.getTokenText());
  System.out.println(showParserVal(yylval));
  b.advanceLexer();
  m.collapse(typ);
  return v;
}

CabalYaccParserVal markWith(IElementType typ, CabalYaccParserVal v) {
  return markWith(typ, v, v);
}

CabalYaccParserVal markWith(IElementType typ, CabalYaccParserVal v1, CabalYaccParserVal v2) {
  PsiBuilder.Marker m = getMarker(v1).precede();
  if (yylval == v2 || b.eof()) m.done(typ);
  else m.doneBefore(typ, getMarker(yylval));
  CabalYaccParserVal result = newParserVal(m, typ, null);
  System.out.println(showParserVal(result));
  return result;
}


CabalYaccParserVal errorWith(String msg, CabalYaccParserVal v) {
  return errorWith(msg, v, v);
}

CabalYaccParserVal errorWith(String msg, CabalYaccParserVal v1, CabalYaccParserVal v2) {
  PsiBuilder.Marker m = getMarker(v1).precede();
  if (yylval == v2 || b.eof()) m.error(msg);
  else m.errorBefore(msg, getMarker(yylval));
  CabalYaccParserVal result = newParserVal(m, null, null);
  System.out.println(showParserVal(result));
  return result;
}

PsiBuilder.Marker getMarker(CabalYaccParserVal v) {
  return ((ValObj)v.obj).m;
}

short getYYVal(IElementType typ) {
  return ((CabalTokenType)typ).yyval();
}

CabalYaccParserVal newParserVal(PsiBuilder.Marker m, IElementType typ, String text) {
  return new CabalYaccParserVal(new ValObj(m, typ, text));
}

String showParserVal(CabalYaccParserVal v) {
  ValObj o = (ValObj)v.obj;
  return "CabalYaccParserVal"
    + " typ: " + o.typ
    + " text: " + (o.text == null ? "null" : "'" + o.text.replace("\n", "\n\n") + "'");
}

static class ValObj {
  final PsiBuilder.Marker m;
  final IElementType typ;
  final String text;

  public ValObj(PsiBuilder.Marker m, IElementType typ, String text) {
    this.m = m;
    this.typ = typ;
    this.text = text;
  }
}
