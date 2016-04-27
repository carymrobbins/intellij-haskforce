package com.haskforce.cabal.psi;

import com.haskforce.cabal.lang.parser.CabalYaccParser;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;

/**
 * Contains all IElementType values returned from Cabal lexers and used by parsers.
 *
 * Note that we have a logical distinction between different kinds of element types -
 *
 *    CabalTokenType - A token returned from the lexer.  Note that we have specific
 *      subtypes of CabalTokenType to identify, to the parser, the kind of token
 *      we are dealing with when we need to handle them in a general case,
 *      e.g. parse any CabalIdentTokenType.
 *
 *    CabalElementType - An element constructed using the parser, usually consisting of
 *      one or more tokens.
 */
public interface CabalTypes {

  IElementType WHITE_SPACE = TokenType.WHITE_SPACE;

  // Tokens specific to the highlighter.

  CabalHighlighterTokenType VALUE_CHAR = new CabalHighlighterTokenType("VALUE_CHAR");
  CabalHighlighterTokenType CONFIG = new CabalHighlighterTokenType("CONFIG");
  CabalHighlighterTokenType KEY = new CabalHighlighterTokenType("KEY");
  CabalHighlighterTokenType CONDITIONAL = new CabalHighlighterTokenType("CONDITIONAL");

  // Tokens specific to the parser.

  CabalElementType IF_EXPR = new CabalElementType("IF_EXPR");
  CabalElementType IF_COND = new CabalElementType("IF_COND");
  CabalElementType THEN_BODY = new CabalElementType("THEN_BODY");
  CabalElementType ELSE_BODY = new CabalElementType("ELSE_BODY");
  CabalElementType LOGICAL_NEG = new CabalElementType("LOGICAL_NEG");

  CabalElementType FREEFORM = new CabalElementType("FREEFORM");
  CabalElementType FREEFORM_LINE = new CabalElementType("FREEFORM_LINE");

  CabalElementType IDENT_LIST = new CabalElementType("IDENT_LIST");
  CabalIdentTokenType IDENT = new CabalIdentTokenType("IDENT", CabalYaccParser.IDENT);

  CabalElementType MODULE_LIST = new CabalElementType("MODULE_LIST");
  CabalElementType MODULE = new CabalElementType("MODULE");
  CabalElementType MODULE_PART = new CabalElementType("MODULE_PART");

  CabalElementType BOOL_VALUE = new CabalElementType("BOOL_VALUE");
  CabalElementType BOOL_LIT = new CabalElementType("BOOL_LIT");
  CabalElementType FUNC_CALL = new CabalElementType("FUNC_CALL");
  CabalElementType FUNC_NAME = new CabalElementType("FUNC_NAME");
  CabalElementType FUNC_ARG = new CabalElementType("FUNC_ARG");

  CabalTokenType COMMENT = new CabalTokenType("COMMENT", CabalYaccParser.COMMENT);

  CabalLayoutTokenType EOL = new CabalLayoutTokenType("EOL", CabalYaccParser.EOL);
  CabalLayoutTokenType INDENT = new CabalLayoutTokenType("INDENT", CabalYaccParser.INDENT);
  CabalLayoutTokenType DEDENT = new CabalLayoutTokenType("DEDENT", CabalYaccParser.DEDENT);

  CabalNumericTokenType NUMBERS = new CabalNumericTokenType("NUMBERS", CabalYaccParser.NUMBERS);

  CabalComparatorTokenType EQ = new CabalComparatorTokenType("==", CabalYaccParser.EQ);
  CabalComparatorTokenType GT = new CabalComparatorTokenType(">", CabalYaccParser.GT);
  CabalComparatorTokenType GTE = new CabalComparatorTokenType(">=", CabalYaccParser.GTE);
  CabalComparatorTokenType LT = new CabalComparatorTokenType("<", CabalYaccParser.LT);
  CabalComparatorTokenType LTE = new CabalComparatorTokenType("<=", CabalYaccParser.LTE);

  CabalLogicalTokenType AND = new CabalLogicalTokenType("AND", CabalYaccParser.AND);
  CabalLogicalTokenType OR = new CabalLogicalTokenType("OR", CabalYaccParser.OR);

  CabalSymbolTokenType LPAREN = new CabalSymbolTokenType("(", CabalYaccParser.LPAREN);
  CabalSymbolTokenType RPAREN = new CabalSymbolTokenType(")", CabalYaccParser.RPAREN);
  CabalSymbolTokenType COLON = new CabalSymbolTokenType("COLON", CabalYaccParser.COLON);
  CabalSymbolTokenType LBRACKET = new CabalSymbolTokenType("[", CabalYaccParser.LBRACKET);
  CabalSymbolTokenType RBRACKET = new CabalSymbolTokenType("]", CabalYaccParser.RBRACKET);
  CabalSymbolTokenType LBRACE = new CabalSymbolTokenType("{", CabalYaccParser.LBRACE);
  CabalSymbolTokenType RBRACE = new CabalSymbolTokenType("}", CabalYaccParser.RBRACE);
  CabalSymbolTokenType DASH = new CabalSymbolTokenType("-", CabalYaccParser.DASH);
  CabalSymbolTokenType DOT = new CabalSymbolTokenType(".", CabalYaccParser.DOT);
  CabalSymbolTokenType COMMA = new CabalSymbolTokenType(",", CabalYaccParser.COMMA);
  CabalSymbolTokenType BANG = new CabalSymbolTokenType("!", CabalYaccParser.BANG);
  // Tabs don't affect the layout but also aren't words, so make them symbols.
  CabalSymbolTokenType TAB = new CabalSymbolTokenType("TAB", CabalYaccParser.TAB);
  // Some other character we didn't account for, let's assume it's a symbol;
  // although, it might not be.
  CabalSymbolTokenType OTHER_CHAR = new CabalSymbolTokenType("OTHER_CHAR", CabalYaccParser.OTHER_CHAR);

  // Keywords

  CabalIdentTokenType WITH = new CabalIdentTokenType("WITH", CabalYaccParser.WITH);
  CabalIdentTokenType TRUE = new CabalIdentTokenType("TRUE", CabalYaccParser.TRUE);
  CabalIdentTokenType FALSE = new CabalIdentTokenType("FALSE", CabalYaccParser.FALSE);
  CabalIdentTokenType IF = new CabalIdentTokenType("IF", CabalYaccParser.IF);
  CabalIdentTokenType ELSE = new CabalIdentTokenType("ELSE", CabalYaccParser.ELSE);
  CabalFlagKeywordTokenType FLAG = new CabalFlagKeywordTokenType("FLAG", CabalYaccParser.FLAG);
  CabalFuncNameTokenType OS = new CabalFuncNameTokenType("OS", CabalYaccParser.OS);
  CabalFuncNameTokenType ARCH = new CabalFuncNameTokenType("ARCH", CabalYaccParser.ARCH);
  CabalFuncNameTokenType IMPL = new CabalFuncNameTokenType("IMPL", CabalYaccParser.IMPL);

  // Top-level fields

  CabalElementType INVALID_FIELD = new CabalElementType("INVALID_FIELD");

  CabalElementType UNKNOWN_FIELD = new CabalElementType("UNKNOWN_FIELD");
  CabalFieldKeyTokenType UNKNOWN_KEY = new CabalFieldKeyTokenType("UNKNOWN_KEY", CabalYaccParser.UNKNOWN_KEY);

  CabalElementType CUSTOM_FIELD = new CabalElementType("CUSTOM_FIELD");
  CabalFieldKeyTokenType CUSTOM_KEY = new CabalFieldKeyTokenType("CUSTOM_KEY", CabalYaccParser.CUSTOM_KEY);

  // TODO: Remove other *NAME and *TYPE fields
  CabalElementType NAME = new CabalElementType("NAME");
  CabalElementType TYPE = new CabalElementType("TYPE");
  CabalElementType VERSION = new CabalElementType("VERSION");
  CabalElementType BRANCH = new CabalElementType("BRANCH");
  CabalElementType DEFAULT = new CabalElementType("DEFAULT");
  CabalElementType MANUAL = new CabalElementType("MANUAL");
  CabalElementType SUBDIR = new CabalElementType("SUBDIR");
  CabalElementType TAG = new CabalElementType("TAG");
  CabalElementType LOCATION = new CabalElementType("LOCATION");

  CabalElementType PKG_NAME = new CabalElementType("PKG_NAME");
  CabalFieldKeyTokenType NAME_KEY = new CabalFieldKeyTokenType("NAME_KEY", CabalYaccParser.NAME_KEY);

  CabalElementType PKG_VERSION = new CabalElementType("PKG_VERSION");
  CabalFieldKeyTokenType VERSION_KEY = new CabalFieldKeyTokenType("VERSION_KEY", CabalYaccParser.VERSION_KEY);

//  CabalElementType VERSION_BRANCH = new CabalElementType("VERSION_BRANCH");
//  CabalElementType VERSION_TAG = new CabalElementType("VERSION_TAG");

  CabalElementType CABAL_VERSION = new CabalElementType("CABAL_VERSION");
  CabalFieldKeyTokenType CABAL_VERSION_KEY = new CabalFieldKeyTokenType("CABAL_VERSION_KEY", CabalYaccParser.CABAL_VERSION_KEY);

  CabalElementType BUILD_TYPE = new CabalElementType("BUILD_TYPE");
  CabalFieldKeyTokenType BUILD_TYPE_KEY = new CabalFieldKeyTokenType("BUILD_TYPE_KEY", CabalYaccParser.BUILD_TYPE_KEY);

  CabalElementType LICENSE = new CabalElementType("LICENSE");
  CabalFieldKeyTokenType LICENSE_KEY = new CabalFieldKeyTokenType("LICENSE_KEY", CabalYaccParser.LICENSE_KEY);

  CabalElementType LICENSE_FILE = new CabalElementType("LICENSE_FILE");
  CabalFieldKeyTokenType LICENSE_FILE_KEY = new CabalFieldKeyTokenType("LICENSE_FILE_KEY", CabalYaccParser.LICENSE_FILE_KEY);

  CabalElementType LICENSE_FILES = new CabalElementType("LICENSE_FILES");
  CabalFieldKeyTokenType LICENSE_FILES_KEY = new CabalFieldKeyTokenType("LICENSE_FILES_KEY", CabalYaccParser.LICENSE_FILES_KEY);

  CabalElementType COPYRIGHT = new CabalElementType("COPYRIGHT");
  CabalFieldKeyTokenType COPYRIGHT_KEY = new CabalFieldKeyTokenType("COPYRIGHT_KEY", CabalYaccParser.COPYRIGHT_KEY);

  CabalElementType AUTHOR = new CabalElementType("AUTHOR");
  CabalFieldKeyTokenType AUTHOR_KEY = new CabalFieldKeyTokenType("AUTHOR_KEY", CabalYaccParser.AUTHOR_KEY);

  CabalElementType MAINTAINER = new CabalElementType("MAINTAINER");
  CabalFieldKeyTokenType MAINTAINER_KEY = new CabalFieldKeyTokenType("MAINTAINER_KEY", CabalYaccParser.MAINTAINER_KEY);

  CabalElementType STABILITY = new CabalElementType("STABILITY");
  CabalFieldKeyTokenType STABILITY_KEY = new CabalFieldKeyTokenType("STABILITY_KEY", CabalYaccParser.STABILITY_KEY);

  CabalElementType HOMEPAGE = new CabalElementType("HOMEPAGE");
  CabalFieldKeyTokenType HOMEPAGE_KEY = new CabalFieldKeyTokenType("HOMEPAGE_KEY", CabalYaccParser.HOMEPAGE_KEY);

  CabalElementType BUG_REPORTS = new CabalElementType("BUG_REPORTS");
  CabalFieldKeyTokenType BUG_REPORTS_KEY = new CabalFieldKeyTokenType("BUG_REPORTS_KEY", CabalYaccParser.BUG_REPORTS_KEY);

  CabalElementType PACKAGE_URL = new CabalElementType("PACKAGE_URL");
  CabalFieldKeyTokenType PACKAGE_URL_KEY = new CabalFieldKeyTokenType("PACKAGE_URL_KEY", CabalYaccParser.PACKAGE_URL_KEY);

  CabalElementType SYNOPSIS = new CabalElementType("SYNOPSIS");
  CabalFieldKeyTokenType SYNOPSIS_KEY = new CabalFieldKeyTokenType("SYNOPSIS_KEY", CabalYaccParser.SYNOPSIS_KEY);

  CabalElementType DESCRIPTION = new CabalElementType("DESCRIPTION");
  CabalFieldKeyTokenType DESCRIPTION_KEY = new CabalFieldKeyTokenType("DESCRIPTION_KEY", CabalYaccParser.DESCRIPTION_KEY);

  CabalElementType CATEGORY = new CabalElementType("CATEGORY");
  CabalFieldKeyTokenType CATEGORY_KEY = new CabalFieldKeyTokenType("CATEGORY_KEY", CabalYaccParser.CATEGORY_KEY);

  CabalElementType TESTED_WITH = new CabalElementType("TESTED_WITH");
  CabalFieldKeyTokenType TESTED_WITH_KEY = new CabalFieldKeyTokenType("TESTED_WITH_KEY", CabalYaccParser.TESTED_WITH_KEY);

  CabalElementType DATA_FILES = new CabalElementType("DATA_FILES");
  CabalFieldKeyTokenType DATA_FILES_KEY = new CabalFieldKeyTokenType("DATA_FILES_KEY", CabalYaccParser.DATA_FILES_KEY);

  CabalElementType DATA_DIR = new CabalElementType("DATA_DIR");
  CabalFieldKeyTokenType DATA_DIR_KEY = new CabalFieldKeyTokenType("DATA_DIR_KEY", CabalYaccParser.DATA_DIR_KEY);

  CabalElementType EXTRA_SOURCE_FILES = new CabalElementType("EXTRA_SOURCE_FILES");
  CabalFieldKeyTokenType EXTRA_SOURCE_FILES_KEY = new CabalFieldKeyTokenType("EXTRA_SOURCE_FILES_KEY", CabalYaccParser.EXTRA_SOURCE_FILES_KEY);

  CabalElementType EXTRA_DOC_FILES = new CabalElementType("EXTRA_DOC_FILES");
  CabalFieldKeyTokenType EXTRA_DOC_FILES_KEY = new CabalFieldKeyTokenType("EXTRA_DOC_FILES_KEY", CabalYaccParser.EXTRA_DOC_FILES_KEY);

  CabalElementType EXTRA_TMP_FILES = new CabalElementType("EXTRA_TMP_FILES");
  CabalFieldKeyTokenType EXTRA_TMP_FILES_KEY = new CabalFieldKeyTokenType("EXTRA_TMP_FILES_KEY", CabalYaccParser.EXTRA_TMP_FILES_KEY);

  // Flag stanza fields

  CabalElementType FLAG_DECL = new CabalElementType("FLAG_DECL");

  CabalElementType FLAG_NAME = new CabalElementType("FLAG_NAME");

  CabalElementType FLAG_DESCR = new CabalElementType("FLAG_DESCR");

  CabalElementType FLAG_DEFAULT = new CabalElementType("FLAG_DEFAULT");
  CabalFieldKeyTokenType DEFAULT_KEY = new CabalFieldKeyTokenType("DEFAULT_KEY", CabalYaccParser.DEFAULT_KEY);

  CabalElementType FLAG_MANUAL = new CabalElementType("FLAG_MANUAL");
  CabalFieldKeyTokenType MANUAL_KEY = new CabalFieldKeyTokenType("MANUAL_KEY", CabalYaccParser.MANUAL_KEY);

  // Shared keys

  // Used by Source-repository, Test-suite, Benchmark
  CabalFieldKeyTokenType TYPE_KEY = new CabalFieldKeyTokenType("TYPE_KEY", CabalYaccParser.TYPE_KEY);

  // Used for Executable, Test-suite, and Benchmark
  CabalElementType MAIN_IS = new CabalElementType("MAIN_IS");
  CabalFieldKeyTokenType MAIN_IS_KEY = new CabalFieldKeyTokenType("MAIN_IS_KEY", CabalYaccParser.MAIN_IS_KEY);

  // Source-repository stanza fields

  CabalElementType SOURCE_REPO = new CabalElementType("SOURCE_REPO");
  CabalStanzaKeyTokenType SOURCE_REPO_KEY = new CabalStanzaKeyTokenType("SOURCE_REPO_KEY", CabalYaccParser.SOURCE_REPO_KEY);
  CabalElementType SOURCE_REPO_NAME = new CabalElementType("SOURCE_REPO_NAME");

  CabalElementType SOURCE_REPO_TYPE = new CabalElementType("SOURCE_REPO_TYPE");

  CabalElementType SOURCE_REPO_LOCATION = new CabalElementType("SOURCE_REPO_LOCATION");
  CabalFieldKeyTokenType LOCATION_KEY = new CabalFieldKeyTokenType("LOCATION_KEY", CabalYaccParser.LOCATION_KEY);

  CabalElementType SOURCE_REPO_MODULE = new CabalElementType("SOURCE_REPO_MODULE");
  CabalFieldKeyTokenType MODULE_KEY = new CabalFieldKeyTokenType("MODULE_KEY", CabalYaccParser.MODULE_KEY);

  CabalElementType SOURCE_REPO_BRANCH = new CabalElementType("SOURCE_REPO_BRANCH");
  CabalFieldKeyTokenType BRANCH_KEY = new CabalFieldKeyTokenType("BRANCH_KEY", CabalYaccParser.BRANCH_KEY);

  CabalElementType SOURCE_REPO_TAG = new CabalElementType("SOURCE_REPO_TAG");
  CabalFieldKeyTokenType TAG_KEY = new CabalFieldKeyTokenType("TAG_KEY", CabalYaccParser.TAG_KEY);

  CabalElementType SOURCE_REPO_SUBDIR = new CabalElementType("SOURCE_REPO_SUBDIR");
  CabalFieldKeyTokenType SUBDIR_KEY = new CabalFieldKeyTokenType("SUBDIR_KEY", CabalYaccParser.SUBDIR_KEY);

  // Build information stanza fields

  CabalElementType BUILD_DEPENDS = new CabalElementType("BUILD_DEPENDS_KEY");
  CabalFieldKeyTokenType BUILD_DEPENDS_KEY = new CabalFieldKeyTokenType("BUILD_DEPENDS_KEY", CabalYaccParser.BUILD_DEPENDS_KEY);
  CabalElementType DEPENDENCIES = new CabalElementType("DEPENDENCIES");
  CabalElementType DEPENDENCY = new CabalElementType("DEPENDENCY");
  CabalElementType DEPENDENCY_NAME = new CabalElementType("DEPENDENCY_NAME");
  CabalElementType DEPENDENCY_VERSION = new CabalElementType("DEPENDENCY_VERSION");

  CabalElementType OTHER_MODULES = new CabalElementType("OTHER_MODULES");
  CabalFieldKeyTokenType OTHER_MODULES_KEY = new CabalFieldKeyTokenType("OTHER_MODULES_KEY", CabalYaccParser.OTHER_MODULES_KEY);

  CabalElementType DEFAULT_LANGUAGE = new CabalElementType("DEFAULT_LANGUAGE");
  CabalFieldKeyTokenType DEFAULT_LANGUAGE_KEY = new CabalFieldKeyTokenType("DEFAULT_LANGUAGE_KEY", CabalYaccParser.DEFAULT_LANGUAGE_KEY);

  CabalElementType OTHER_LANGUAGES = new CabalElementType("OTHER_LANGUAGES");
  CabalFieldKeyTokenType OTHER_LANGUAGES_KEY = new CabalFieldKeyTokenType("OTHER_LANGUAGES_KEY", CabalYaccParser.OTHER_LANGUAGES_KEY);

  CabalElementType DEFAULT_EXTENSIONS = new CabalElementType("DEFAULT_EXTENSIONS");
  CabalFieldKeyTokenType DEFAULT_EXTENSIONS_KEY = new CabalFieldKeyTokenType("DEFAULT_EXTENSIONS_KEY", CabalYaccParser.DEFAULT_EXTENSIONS_KEY);

  CabalElementType OTHER_EXTENSIONS = new CabalElementType("OTHER_EXTENSIONS");
  CabalFieldKeyTokenType OTHER_EXTENSIONS_KEY = new CabalFieldKeyTokenType("OTHER_EXTENSIONS_KEY", CabalYaccParser.OTHER_EXTENSIONS_KEY);

  CabalElementType HS_SOURCE_DIRS = new CabalElementType("HS_SOURCE_DIRS");
  CabalFieldKeyTokenType HS_SOURCE_DIRS_KEY = new CabalFieldKeyTokenType("HS_SOURCE_DIRS_KEY", CabalYaccParser.HS_SOURCE_DIRS_KEY);

  CabalElementType EXTENSIONS = new CabalElementType("EXTENSIONS");
  CabalFieldKeyTokenType EXTENSIONS_KEY = new CabalFieldKeyTokenType("EXTENSIONS_KEY", CabalYaccParser.EXECUTABLE_KEY);

  CabalElementType BUILD_TOOLS = new CabalElementType("BUILD_TOOLS");
  CabalFieldKeyTokenType BUILD_TOOLS_KEY = new CabalFieldKeyTokenType("BUILD_TOOLS_KEY", CabalYaccParser.BUILD_TOOLS_KEY);

  CabalElementType BUILDABLE = new CabalElementType("BUILDABLE");
  CabalFieldKeyTokenType BUILDABLE_KEY = new CabalFieldKeyTokenType("BUILDABLE_KEY", CabalYaccParser.BUILDABLE_KEY);

  CabalElementType GHC_OPTIONS = new CabalElementType("GHC_OPTIONS");
  CabalFieldKeyTokenType GHC_OPTIONS_KEY = new CabalFieldKeyTokenType("GHC_OPTIONS_KEY", CabalYaccParser.GHC_OPTIONS_KEY);

  CabalElementType GHC_PROF_OPTIONS = new CabalElementType("GHC_PROF_OPTIONS");
  CabalFieldKeyTokenType GHC_PROF_OPTIONS_KEY = new CabalFieldKeyTokenType("GHC_PROF_OPTIONS_KEY", CabalYaccParser.GHC_PROF_OPTIONS_KEY);

  CabalElementType GHC_SHARED_OPTIONS = new CabalElementType("GHC_SHARED_OPTIONS");
  CabalFieldKeyTokenType GHC_SHARED_OPTIONS_KEY = new CabalFieldKeyTokenType("GHC_SHARED_OPTIONS_KEY", CabalYaccParser.GHC_SHARED_OPTIONS_KEY);

  CabalElementType INCLUDES = new CabalElementType("INCLUDES");
  CabalFieldKeyTokenType INCLUDES_KEY = new CabalFieldKeyTokenType("INCLUDES_KEY", CabalYaccParser.INCLUDES_KEY);

  CabalElementType INSTALL_INCLUDES = new CabalElementType("INSTALL_INCLUDES");
  CabalFieldKeyTokenType INSTALL_INCLUDES_KEY = new CabalFieldKeyTokenType("INSTALL_INCLUDES_KEY", CabalYaccParser.INSTALL_INCLUDES_KEY);

  CabalElementType INCLUDE_DIRS = new CabalElementType("INCLUDE_DIRS");
  CabalFieldKeyTokenType INCLUDE_DIRS_KEY = new CabalFieldKeyTokenType("INCLUDE_DIRS_KEY", CabalYaccParser.INCLUDE_DIRS_KEY);

  CabalElementType C_SOURCES = new CabalElementType("C_SOURCES");
  CabalFieldKeyTokenType C_SOURCES_KEY = new CabalFieldKeyTokenType("C_SOURCES_KEY", CabalYaccParser.C_SOURCES_KEY);

  CabalElementType JS_SOURCES = new CabalElementType("JS_SOURCES");
  CabalFieldKeyTokenType JS_SOURCES_KEY = new CabalFieldKeyTokenType("JS_SOURCES_KEY", CabalYaccParser.JS_SOURCES_KEY);

  CabalElementType EXTRA_LIBRARIES = new CabalElementType("EXTRA_LIBRARIES");
  CabalFieldKeyTokenType EXTRA_LIBRARIES_KEY = new CabalFieldKeyTokenType("EXTRA_LIBRARIES_KEY", CabalYaccParser.EXTRA_LIBRARIES_KEY);

  CabalElementType EXTRA_GHCI_LIBRARIES = new CabalElementType("EXTRA_GHCI_LIBRARIES");
  CabalFieldKeyTokenType EXTRA_GHCI_LIBRARIES_KEY = new CabalFieldKeyTokenType("EXTRA_GHCI_LIBRARIES_KEY", CabalYaccParser.EXTRA_GHCI_LIBRARIES_KEY);

  CabalElementType EXTRA_LIB_DIRS = new CabalElementType("EXTRA_LIB_DIRS");
  CabalFieldKeyTokenType EXTRA_LIB_DIRS_KEY = new CabalFieldKeyTokenType("EXTRA_LIB_DIRS_KEY", CabalYaccParser.EXTRA_LIB_DIRS_KEY);

  CabalElementType CC_OPTIONS = new CabalElementType("CC_OPTIONS");
  CabalFieldKeyTokenType CC_OPTIONS_KEY = new CabalFieldKeyTokenType("CC_OPTIONS_KEY", CabalYaccParser.CC_OPTIONS_KEY);

  CabalElementType CPP_OPTIONS = new CabalElementType("CPP_OPTIONS");
  CabalFieldKeyTokenType CPP_OPTIONS_KEY = new CabalFieldKeyTokenType("CPP_OPTIONS_KEY", CabalYaccParser.CPP_OPTIONS_KEY);

  CabalElementType LD_OPTIONS = new CabalElementType("LD_OPTIONS");
  CabalFieldKeyTokenType LD_OPTIONS_KEY = new CabalFieldKeyTokenType("LD_OPTIONS_KEY", CabalYaccParser.LD_OPTIONS_KEY);

  CabalElementType PKGCONFIG_DEPENDS = new CabalElementType("PKGCONFIG_DEPENDS");
  CabalFieldKeyTokenType PKGCONFIG_DEPENDS_KEY = new CabalFieldKeyTokenType("PKGCONFIG_DEPENDS_KEY", CabalYaccParser.PKGCONFIG_DEPENDS_KEY);

  CabalElementType FRAMEWORKS = new CabalElementType("FRAMEWORKS");
  CabalFieldKeyTokenType FRAMEWORKS_KEY = new CabalFieldKeyTokenType("FRAMEWORKS_KEY", CabalYaccParser.FRAMEWORKS_KEY);

  // Library stanza

  CabalElementType LIBRARY = new CabalElementType("LIBRARY");
  CabalStanzaKeyTokenType LIBRARY_KEY = new CabalStanzaKeyTokenType("LIBRARY_KEY", CabalYaccParser.LIBRARY_KEY);

  CabalElementType EXPOSED_MODULES = new CabalElementType("EXPOSED_MODULES");
  CabalFieldKeyTokenType EXPOSED_MODULES_KEY = new CabalFieldKeyTokenType("EXPOSED_MODULES_KEY", CabalYaccParser.EXPOSED_MODULES_KEY);

  CabalElementType EXPOSED = new CabalElementType("EXPOSED");
  CabalFieldKeyTokenType EXPOSED_KEY = new CabalFieldKeyTokenType("EXPOSED_KEY", CabalYaccParser.EXPOSED_KEY);

  CabalElementType REEXPORTED_MODULES = new CabalElementType("REEXPOSED_MODULES");
  CabalFieldKeyTokenType REEXPORTED_MODULES_KEY = new CabalFieldKeyTokenType("REEXPOSED_MODULES_KEY", CabalYaccParser.REEXPORTED_MODULES_KEY);

  // Executable stanza

  CabalElementType EXECUTABLE = new CabalElementType("EXECUTABLE");
  CabalStanzaKeyTokenType EXECUTABLE_KEY = new CabalStanzaKeyTokenType("EXECUTABLE_KEY", CabalYaccParser.EXECUTABLE_KEY);
  CabalElementType EXECUTABLE_NAME = new CabalElementType("EXECUTABLE_NAME");

  // Test-suite stanza

  CabalElementType TEST_SUITE = new CabalElementType("TEST_SUITE");
  CabalStanzaKeyTokenType TEST_SUITE_KEY = new CabalStanzaKeyTokenType("TEST_SUITE_KEY", CabalYaccParser.TEST_SUITE_KEY);
  CabalElementType TEST_SUITE_NAME = new CabalElementType("TEST_SUITE_NAME");

  CabalElementType TEST_SUITE_TYPE = new CabalElementType("TEST_SUITE_TYPE");

  // Benchmark stanza

  CabalElementType BENCHMARK = new CabalElementType("BENCHMARK");
  CabalStanzaKeyTokenType BENCHMARK_KEY = new CabalStanzaKeyTokenType("BENCHMARK_KEY", CabalYaccParser.BENCHMARK_KEY);
  CabalElementType BENCHMARK_NAME = new CabalElementType("BENCHMARK_NAME");

  CabalElementType BENCHMARK_TYPE = new CabalElementType("BENCHMARK_TYPE");
}
