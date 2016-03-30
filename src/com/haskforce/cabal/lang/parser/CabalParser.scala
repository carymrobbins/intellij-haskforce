package com.haskforce.cabal.lang.parser

import scala.annotation.tailrec

import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType

import com.haskforce.cabal.lang.psi._
import com.haskforce.cabal.lang.psi.CabalTypes._

final class CabalParser extends PsiParser {

  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    new CabalPsiBuilder(builder).doParse(root)
  }
}

final class CabalPsiBuilder(builder: PsiBuilder) extends PsiBuilderAdapter(builder) {

  val DEBUG = false

  def doParse(root: IElementType): ASTNode = {
    val marker = mark()
    while (!eof()) parseFieldOrStanza()
    marker.done(root)
    getTreeBuilt
  }

  def parseFieldOrStanza(): Unit = {
    if (topLevelField() || parseStanza()) return
    // Attempt to recover by reporting an error and just consuming the line.
    errorWith("Unexpected token: " + getTokenType) {
      advanceWhile(getTokenType != EOL) {}
      if (getTokenType == EOL) remapAdvance(WHITE_SPACE)
    }
  }

  def parseStanza(): Boolean = (
    flagDecl()
    || library()
    || executable()
    || testSuite()
    || benchmark()
    || sourceRepo()
  )

  def library(): Boolean = stanza(
    "library", LIBRARY, LIBRARY_KEY, noStanzaArgs, libraryField
  )

  def libraryField(): Boolean = (
    buildInfoField()
    || field(EXPOSED_MODULES, EXPOSED_MODULES_KEY, moduleList)
    || field(EXPOSED, EXPOSED_KEY, freeform)
    || field(REEXPORTED_MODULES, REEXPORTED_MODULES_KEY, freeform)
    || field(REEXPORTED_MODULES, REEXPORTED_MODULES_KEY, freeform)
  )

  def executable(): Boolean = stanza(
    "executable", EXECUTABLE, EXECUTABLE_KEY,
    stanzaNameArg(EXECUTABLE_NAME), executableField
  )

  def executableField(): Boolean = (
    buildInfoField()
    || mainIsField()
  )

  def mainIsField(): Boolean = field(MAIN_IS, MAIN_IS_KEY, freeform)

  def testSuite(): Boolean = stanza(
    "test-suite", TEST_SUITE, TEST_SUITE_KEY,
    stanzaNameArg(TEST_SUITE_NAME), testSuiteField
  )

  def testSuiteField(): Boolean = (
    buildInfoField()
    || mainIsField()
    || field(TEST_SUITE_TYPE, TYPE_KEY, freeform)
  )

  def benchmark(): Boolean = stanza(
    "benchmark", BENCHMARK, BENCHMARK_KEY,
    stanzaNameArg(BENCHMARK_NAME), benchmarkField
  )

  def benchmarkField(): Boolean = (
    buildInfoField()
    || mainIsField()
    || field(BENCHMARK_TYPE, TYPE_KEY, freeform)
  )

  def sourceRepo(): Boolean = stanza(
    "source-repository", SOURCE_REPO, SOURCE_REPO_KEY,
    stanzaNameArg(SOURCE_REPO_NAME), sourceRepoField
  )

  def sourceRepoField(): Boolean = (
    field(SOURCE_REPO_TYPE, TYPE_KEY, freeform)
    || field(SOURCE_REPO_LOCATION, LOCATION_KEY, freeform)
    || field(SOURCE_REPO_MODULE, MODULE_KEY, freeform)
    || field(SOURCE_REPO_BRANCH, BRANCH_KEY, freeform)
    || field(SOURCE_REPO_TAG, TAG_KEY, freeform)
    || field(SOURCE_REPO_SUBDIR, SUBDIR_KEY, freeform)
  )

  def flagDecl(): Boolean = stanza(
    "flag", FLAG_DECL, FLAG, stanzaNameArg(FLAG_NAME), flagField
  )

  def flagField(): Boolean = (
    field(FLAG_DESCR, DESCRIPTION_KEY, freeform)
    || field(FLAG_DEFAULT, DEFAULT_KEY, boolValue)
    || field(FLAG_MANUAL, MANUAL_KEY, boolValue)
  )

  val noStanzaArgs = () => {}

  def stanzaNameArg(el: CabalStanzaArgTokenType)(): Unit = getTokenType match {
    case _: CabalIdentTokenType => remapAdvance(el)
    case _ => error(s"Expected name argument")
  }

  def stanza
      (stanzaType: String,
       el: CabalElementType,
       k: CabalStanzaKeyTokenType,
       argsParser: () => Unit,
       fieldParser: () => Boolean)
      : Boolean = {
    if (getTokenType != k) return false
    val m = mark()
    advanceLexer()
    argsParser()
    if (!eof() && getTokenType != EOL && getTokenType != LBRACE) {
      errorWith(s"Unexpected $stanzaType argument") {
        advanceWhile(getTokenType != EOL && getTokenType != LBRACE) {}
      }
    }
    stanzaBody(stanzaType, fieldParser)
    m.done(el)
    true
  }

  def stanzaBraceBody(stanzaType: String, fieldParser: () => Boolean): Unit = {
    var foundRBrace = false
    parseWhile(!foundRBrace) {
      getTokenType match {
        case _: CabalLayoutTokenType => remapAdvance(WHITE_SPACE)
        case RBRACE => advanceLexer(); foundRBrace = true
        case _ =>
          if (!(ifExpr(stanzaType, fieldParser) || fieldParser() || invalidField(stanzaType))) {
            error(s"Unexpected token in $stanzaType stanza: " + getTokenType)
          }
      }
    }
    if (!foundRBrace) error("Missing }")
    if (getTokenType == EOL) remapAdvance(WHITE_SPACE)
  }

  def stanzaIndentBody(stanzaType: String, fieldParser: () => Boolean): Unit = {
    if (getTokenType == INDENT) {
      var indent = 0
      advanceWhile(getTokenType == INDENT) {
        remapCurrentToken(WHITE_SPACE)
        indent += 1
      }
      var break = false
      parseWhile(!break && indent > 0) {
        assert(getTokenType != INDENT, "Unexpected INDENT")
        if (!(ifExpr(stanzaType, fieldParser) || fieldParser() || invalidField(stanzaType))) {
          error(s"Unexpected token in $stanzaType stanza: " + getTokenType)
          break = true
        }
        advanceWhile(indent > 0 && getTokenType == DEDENT) { indent -= 1 }
      }
    }
  }

  def stanzaBody(stanzaType: String, fieldParser: () => Boolean): Unit = getTokenType match {
    case LBRACE => advanceLexer(); stanzaBraceBody(stanzaType, fieldParser)
    case EOL => remapAdvance(WHITE_SPACE); stanzaIndentBody(stanzaType, fieldParser)
    case _ => error("Expected { or end of line")
  }

  def ifExpr(stanzaType: String, fieldParser: () => Boolean): Boolean = {
    if (getTokenType != IF) return false
    val m = mark()
    advanceLexer()
    ifCond()
    markWith(THEN_BODY) { stanzaBody(stanzaType, fieldParser) }
    if (getTokenType == ELSE) {
      advanceLexer()
      markWith(ELSE_BODY) { stanzaBody(stanzaType, fieldParser) }
    }
    m.done(IF_EXPR)
    true
  }

  def ifCond(): Unit = {
    val m = mark()
    boolExpr()
    m.done(IF_COND)
  }

  def ifBody(stanzaType: String, fieldParser: () => Boolean): Unit = {
    if (getTokenType != INDENT) {
      error("Expected indent")
    } else {
      var indent = 0
      advanceWhile(getTokenType == INDENT) {
        remapCurrentToken(WHITE_SPACE)
        indent += 1
      }
      var break = false
      parseWhile(!break && indent > 0) {
        assert(getTokenType != INDENT, "Unexpected INDENT")
        if (!(ifExpr(stanzaType, fieldParser) || fieldParser() || invalidField(stanzaType))) {
          error(s"Unexpected token in $stanzaType stanza: " + getTokenType)
          break = true
        }
        advanceWhile(indent > 0 && getTokenType == DEDENT) { indent -= 1 }
      }
    }
  }

  def boolExpr(): Unit = {
    if(boolLit() || funcCall() || negation()) return
    errorAdvance("Invalid boolean expression")
  }

  def negation(): Boolean = {
    if (getTokenType != BANG) return false
    val m = mark()
    advanceLexer()
    boolExpr()
    m.done(LOGICAL_NEG)
    true
  }

  def boolLit(): Boolean = getTokenType match {
    case TRUE | FALSE =>
      val m = mark()
      advanceLexer()
      m.done(BOOL_LIT)
      true

    case _ => false
  }

  def funcCall(): Boolean = getTokenType match {
    case _: CabalIdentTokenType =>
      markWith(FUNC_CALL) {
        markWith(FUNC_NAME) {
          if (getTokenType.isInstanceOf[CabalFuncLikeTokenType]) {
            advanceLexer()
          } else {
            errorWith("Invalid function") { advanceLexer() }
          }
        }
        if (getTokenType != LPAREN) error("Expected (") else advanceLexer()
        markWith(FUNC_ARG) {
          advanceWhile(getTokenType != RPAREN && getTokenType != EOL) {}
        }
        if (getTokenType != RPAREN) error("Expected )") else advanceLexer()
      }
      true

    case _ => false
  }

  def topLevelField(): Boolean = (
    field(PKG_NAME, NAME_KEY, freeform)
    || field(PKG_VERSION, VERSION_KEY, freeform)
    || field(CABAL_VERSION, CABAL_VERSION_KEY, freeform)
    || field(BUILD_TYPE, BUILD_TYPE_KEY, freeform)
    || field(LICENSE, LICENSE_KEY, freeform)
    || field(LICENSE_FILE, LICENSE_FILE_KEY, freeform)
    || field(LICENSE_FILES, LICENSE_FILES_KEY, freeform)
    || field(COPYRIGHT, COPYRIGHT_KEY, freeform)
    || field(AUTHOR, AUTHOR_KEY, freeform)
    || field(MAINTAINER, MAINTAINER_KEY, freeform)
    || field(STABILITY, STABILITY_KEY, freeform)
    || field(HOMEPAGE, HOMEPAGE_KEY, freeform)
    || field(BUG_REPORTS, BUG_REPORTS_KEY, freeform)
    || field(PACKAGE_URL, PACKAGE_URL_KEY, freeform)
    || field(SYNOPSIS, SYNOPSIS_KEY, freeform)
    || field(DESCRIPTION, DESCRIPTION_KEY, freeform)
    || field(CATEGORY, CATEGORY_KEY, freeform)
    || field(TESTED_WITH, TESTED_WITH_KEY, freeform)
    || field(DATA_FILES, DATA_FILES_KEY, freeform)
    || field(DATA_DIR, DATA_DIR_KEY, freeform)
    || field(EXTRA_SOURCE_FILES, EXTRA_SOURCE_FILES_KEY, freeform)
    || field(EXTRA_DOC_FILES, EXTRA_DOC_FILES_KEY, freeform)
    || field(EXTRA_TMP_FILES, EXTRA_TMP_FILES_KEY, freeform)
    || field(CUSTOM_FIELD, CUSTOM_KEY, freeform)
    || invalidField("main")
  )

  def buildInfoField(): Boolean = (
    field(BUILD_DEPENDS, BUILD_DEPENDS_KEY, dependencies)
    || field(OTHER_MODULES, OTHER_MODULES_KEY, moduleList)
    || field(DEFAULT_LANGUAGE, DEFAULT_LANGUAGE_KEY, freeform)
    || field(OTHER_LANGUAGES, OTHER_LANGUAGES_KEY, identList)
    || field(DEFAULT_EXTENSIONS, DEFAULT_EXTENSIONS_KEY, identList)
    || field(OTHER_EXTENSIONS, OTHER_EXTENSIONS_KEY, identList)
    || field(HS_SOURCE_DIRS, HS_SOURCE_DIRS_KEY, freeform)
    || field(EXTENSIONS, EXTENSIONS_KEY, identList)
    || field(BUILD_TOOLS, BUILD_TOOLS_KEY, freeform)
    || field(BUILDABLE, BUILDABLE_KEY, freeform)
    || field(GHC_OPTIONS, GHC_OPTIONS_KEY, ghcOptions)
    || field(GHC_PROF_OPTIONS, GHC_PROF_OPTIONS_KEY, ghcOptions)
    || field(GHC_SHARED_OPTIONS, GHC_SHARED_OPTIONS_KEY, ghcOptions)
    || field(INCLUDES, INCLUDES_KEY, freeform)
    || field(INSTALL_INCLUDES, INSTALL_INCLUDES_KEY, freeform)
    || field(INCLUDE_DIRS, INCLUDE_DIRS_KEY, freeform)
    || field(C_SOURCES, C_SOURCES_KEY, freeform)
    || field(JS_SOURCES, JS_SOURCES_KEY, freeform)
    || field(EXTRA_LIBRARIES, EXTRA_LIBRARIES_KEY, freeform)
    || field(EXTRA_GHCI_LIBRARIES, EXTRA_GHCI_LIBRARIES_KEY, freeform)
    || field(EXTRA_LIB_DIRS, EXTRA_LIB_DIRS_KEY, freeform)
    || field(CC_OPTIONS, CC_OPTIONS_KEY, freeform)
    || field(CPP_OPTIONS, CPP_OPTIONS_KEY, freeform)
    || field(LD_OPTIONS, LD_OPTIONS_KEY, freeform)
    || field(PKGCONFIG_DEPENDS, PKGCONFIG_DEPENDS_KEY, freeform)
    || field(FRAMEWORKS, FRAMEWORKS_KEY, freeform)
  )
  /**
   * Should be called after all valid fields have been tried.
   * If any CabalKeyElementType is found, applies an error element.
   */
  def invalidField(stanzaType: String): Boolean = getTokenType match {
    case _: CabalFieldKeyTokenType =>
      val m = mark()
      errorAdvance(s"Field not supported in $stanzaType stanza")
      expectColon()
      freeform()
      m.done(INVALID_FIELD)
      true

    case _ => false
  }

  def field(el: CabalElementType, k: CabalFieldKeyTokenType, p: () => Unit): Boolean = {
    if (getTokenType != k) return false
    val m = mark()
    advanceLexer()
    expectColon()
    p()
    m.done(el)
    true
  }

  def ghcOptions(): Unit = {
    val m = mark()
    indentContext { _ => ghcOption() }
    m.done(IDENT_LIST)
  }

  def ghcOption(): Unit = {
    val m = mark()
    var break = false
    parseWhile(!break) {
      val next = rawLookup(1)
      if (next == WHITE_SPACE || next.isInstanceOf[CabalLayoutTokenType]) {
        break = true
      }
      advanceLexer()
    }
    m.collapse(IDENT)
  }

  def dependencies(): Unit = {
    val m = mark()
    var lookForComma = false
    indentContext {
      case COMMA =>
        if (!lookForComma) error("Unexpected comma")
        advanceLexer()

      case _: CabalIdentTokenType =>
        dependency()
        lookForComma = true

      case other => errorAdvance("Expected dependency")
    }
    m.done(DEPENDENCIES)
  }

  def dependency(): Unit = {
    assert(getTokenType.isInstanceOf[CabalIdentTokenType], "Unexpected token: " + getTokenType)
    val m = mark()
    remapAdvance(DEPENDENCY_NAME)
    dependencyVersion()
    m.done(DEPENDENCY)
  }

  def dependencyVersion(): Unit = {
    getTokenType match {
      case NUMBERS =>
        val m = mark()
        advanceLexer()
        m.done(DEPENDENCY_VERSION)

      case _: CabalComparatorTokenType =>
        val m = mark()
        var break = false
        parseWhile(!break) {
          getTokenType match {
            case _: CabalComparatorTokenType =>
              advanceLexer()
              if (getTokenType != NUMBERS) error("Expected numbers")
              advanceLexer()
              getTokenType match {
                case _: CabalLogicalTokenType => advanceLexer()
                case _ => break = true
              }

            case _ => break = true
          }
        }
        m.done(DEPENDENCY_VERSION)

      case DASH =>
        val m = mark()
        advanceLexer()
        if (getTokenType.isInstanceOf[CabalIdentTokenType]) {
          if (getTokenText == "any") {
            advanceLexer()
          } else {
            errorAdvance("Expected -any")
          }
        } else {
          error("Expected -any")
        }
        m.done(DEPENDENCY_VERSION)

      case LPAREN =>
        val m = mark()
        advanceLexer()
        var break = false
        parseWhile(!break) {
          dependencyVersion()
          if (getTokenType != RPAREN) {
            error("Expected )")
          } else {
            advanceLexer()
          }
          if (getTokenType.isInstanceOf[CabalLogicalTokenType]) {
            advanceLexer()
          } else {
            break = true
          }
          if (getTokenType == LPAREN) {
            advanceLexer()
          } else {
            break = true
          }
        }
        m.done(DEPENDENCY_VERSION)


      case _ => // done
    }
  }

  def boolValue(): Unit = {
    var found = false
    var err = false
    indentContext {
      case (TRUE | FALSE) if !found =>
        found = true
        val m = mark()
        advanceLexer()
        m.done(BOOL_VALUE)

      case _ if !err =>
        err = true
        error("Invalid boolean")
        advanceLexer()

      case _ => advanceLexer()
    }
  }

  def freeform(): Unit = {
    val m = mark()
    indentContext { _ => valueLine() }
    m.done(FREEFORM)
  }

  def identList(): Unit = {
    val m = mark()
    indentContext {
      case _: CabalIdentTokenType => remapAdvance(IDENT)
      case COMMA => advanceLexer() // skip commas
      case _ => errorAdvance("Expected identifier")
    }
    m.done(IDENT_LIST)
  }

  def moduleList(): Unit = {
    val m = mark()
    indentContext {
      case _: CabalIdentTokenType => module()
      case COMMA => advanceLexer() // skip commas
      case other => errorAdvance("Expected module")
    }
    m.done(MODULE_LIST)
  }

  def module(): Boolean = {
    assert(getTokenType.isInstanceOf[CabalIdentTokenType], "Unexpected token: " + getTokenType)
    val m = mark()
    remapAdvance(MODULE_PART)
    var break = false
    parseWhile(!break && getTokenType == DOT) {
      advanceLexer()
      if (!getTokenType.isInstanceOf[CabalIdentTokenType]) {
        error("Expected module")
        break = true
      } else {
        remapAdvance(MODULE_PART)
      }
    }
    m.done(MODULE)
    true
  }

  /** Runs a parser on each eligible element in an indentation context. */
  def indentContext(p: IElementType => Unit): Unit = {
    var indent = 0
    var newLine = false
    parseWhile(!newLine || indent > 0) {
      getTokenType match {
        case EOL =>
          remapAdvance(WHITE_SPACE)
          newLine = true
          if (getTokenType == INDENT) {
            remapAdvance(WHITE_SPACE)
            indent += 1
          }

        case INDENT =>
          remapAdvance(WHITE_SPACE)
          indent += 1

        case DEDENT =>
          remapAdvance(WHITE_SPACE)
          indent -= 1

        case other => p(other)
      }
    }
  }

  def valueLine(): Unit = {
    val m = mark()
    if (getTokenType == INDENT || getTokenType == DEDENT) {
      error(s"Unexpected token in value line: " + getTokenType)
    }
    advanceWhile(getTokenType != EOL) {}
    m.collapse(FREEFORM_LINE)
  }

  def expectColon(): Unit = {
    if (getTokenType == COLON) advanceLexer()
    else error("Missing colon")
  }

  def consumeEOL(): Boolean = {
    if (getTokenType == EOL) {
      remapAdvance(WHITE_SPACE)
      true
    } else {
      error("Expected end of line")
      false
    }
  }

  def remapAdvance(typ: IElementType): Unit = {
    remapCurrentToken(typ)
    advanceLexer()
  }

  def errorAdvance(msg: String): Unit = {
    errorWith(msg) { advanceLexer() }
  }

  /** Safer while loop that always checks eof() to avoid infinite loops. */
  def parseWhile(cond: => Boolean)(block: => Unit): Unit = {
    while (!eof() && cond) {
      block
    }
  }

  /** Safer while loop that always checks eof() and advances to avoid infinite loops. */
  def advanceWhile(cond: => Boolean)(block: => Unit): Unit = {
    while (!eof() && cond) {
      block
      advanceLexer()
    }
  }

  def markWith[A](el: CabalElementType)(block: => A): A = {
    val m = mark()
    val result = block
    m.done(el)
    result
  }

  def errorWith[A](msg: String)(block: => A): A = {
    val m = mark()
    val result = block
    m.error(msg)
    result
  }

  def assert(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new AssertionError(msg)
  }

  override def advanceLexer(): Unit = {
    super.advanceLexer()
    if (DEBUG && !eof()) println("<- " + getTokenType + ": '" + getTokenText.replace("\n", "\\n") + "'")
  }
}
