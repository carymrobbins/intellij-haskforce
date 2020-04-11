package com.haskforce.haskell.lang.parser

import com.haskforce.haskell.lang.parser.gen.{HaskellParser2020Elements => E}
import com.haskforce.haskell.lang.parser.{HaskellTokenTypes2020 => T}
import com.haskforce.psi.HaskellTokenType
import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

final class HaskellParser2020 extends PsiParser {
  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    new HaskellPsiBuilder(builder).parseRoot(root)
  }
}

private final class HaskellPsiBuilder(builder: PsiBuilder) extends PsiBuilderAdapter(builder) {

  import HaskellPsiBuilder.{MarkResult, Marker, Parse}

  // In prod we want this to be 'false' but can be useful as 'true' for testing.
  private val DEBUG = true
  // In prod we want this to be 'true' but can be useful as 'false' for testing.
  private val PARSE_UNPARSED = false

  private def debug(message: => String): Unit = {
    if (DEBUG) {
      System.err.println(
        message
          + s"; offset=$getCurrentOffset"
          + s"; token='$getTokenType'"
          + s"; text='$getTokenText'"
      )
    }
  }

  def parseRoot(root: IElementType): ASTNode = {
    val rootMarker = mark()
    pModule.run()
    if (PARSE_UNPARSED) {
      remapUnparsedSyntheticWhitespace()
      handleUnparsedInput()
    }
    rootMarker.done(root)
    getTreeBuilt
  }

  private def remapUnparsedSyntheticWhitespace(): Unit = {
    if (eof()) return
    while (isSyntheticWhitespace(getTokenType)) {
      debug("Remapping unparsed synthetic whitespace")
      remapCurrentToken(TokenType.WHITE_SPACE)
      advanceLexer()
    }
  }

  private def isSyntheticWhitespace(t: IElementType): Boolean = t match {
    case T.WHITESPACELBRACETOK => true
    case T.WHITESPACERBRACETOK => true
    case T.WHITESPACESEMITOK => true
    case _ => false
  }

  private def handleUnparsedInput(): Unit = {
    if (eof()) return
    debug("Encountered unparsed input; parsing as UNKNOWN")
    withMark { m =>
      while (!eof()) advanceLexer()
      m.done(E.UNKNOWN)
    }.run()
    ()
  }

  private def pModule = withMark { m =>
    pModuleDecl.run()
    pModuleBody.run()
    m.done(E.MODULE)
  }

  private def pModuleBody = Parse {
    val lbrace = getTokenType
    val optRBrace: Option[HaskellTokenType] = lbrace match {
      case T.WHITESPACELBRACETOK =>
        remapCurrentToken(TokenType.WHITE_SPACE)
        advanceLexer()
        Some(T.WHITESPACERBRACETOK)
      case T.LBRACE =>
        advanceLexer()
        Some(T.RBRACE)
      case _ =>
        debug("Unexpectedly found no real nor synthetic lbrace in module body")
        None
    }
    def end(): Boolean = {
      eof() ||
        optRBrace.contains(getTokenType) && lookAhead(1) == null ||
        optRBrace.isEmpty && getTokenType == null
    }
    while (!end()) pModuleBodyItem.run()
    if (optRBrace.contains(getTokenType)) advanceLexer()
    true
  }

  private def pModuleBodyItem = {
    pImportStmt
      .orElse(pUnknownThroughEOL("pModuleBodyItem"))
  }

  //noinspection SameParameterValue
  private def pUnknownThroughEOL(label: => String) = withMark { m =>
    debug(s"Failed to parse at $label; parsing as UNKNOWN through EOL")
    def getCurrentLine: Int = {
      StringUtil.offsetToLineColumn(getOriginalText, getCurrentOffset).line
    }
    val line = getCurrentLine
    while (getTokenType != null && getCurrentLine == line) advanceLexer()
    m.done(E.UNKNOWN)
  }

  private def pImportStmt = parseIfToken(T.IMPORT) { m =>
    // Consume 'qualified' if it exists.
    if (getTokenType == T.QUALIFIED) advanceLexer()
    if (!pQConid.run()) {
      m.error("Missing module name")
    } else {
      pImportAlias.run()
      pImportExplicits(hiding = false).run()
      if (getTokenType == T.HIDING) {
        advanceLexer()
        pImportExplicits(hiding = true).run()
      }
      m.done(E.IMPORT_STMT)
    }
  }

  private def pImportAlias = withMark { m =>
    if (!pQConid.run()) {
      m.error("Expected import alias name")
    } else {
      m.done(E.IMPORT_ALIAS)
    }
  }

  private def pImportExplicits(hiding: Boolean) = parseIfToken(T.LPAREN) { m =>
    while (getTokenType != T.RPAREN && pImportExplicit(hiding).run()) {
      while (getTokenType == T.COMMA) advanceLexer()
    }
    if (getTokenType == T.RPAREN) advanceLexer()
    m.done(if (hiding) E.IMPORT_HIDDENS else E.IMPORT_EXPLICITS)
  }

  private def pImportExplicit(hiding: Boolean) = {
    pWrapWith(
      if (hiding) E.IMPORT_HIDDEN else E.IMPORT_EXPLICIT,
      pImportItem
    )
  }

  private def pImportItem = {
    pWrapWith(E.IMPORT_ITEM_TYPE_CONSYM, pImportType(pConid))
      .orElse(pWrapWith(E.IMPORT_ITEM_TYPE_CONSYM, pImportType(pConsym)))
      .orElse(pWrapWith(E.IMPORT_ITEM_VARID, pVarid))
      .orElse(pWrapWith(E.IMPORT_ITEM_VARSYM, pVarsym))
  }

  private def pImportType(p: Parse) = Parse {
    if (!p.run()) {
      false
    } else {
      pImportMembers.run() // ok if it fails
      true
    }
  }

  private def pImportMembers = parseIfToken(T.LPAREN) { m =>
    while (pImportMember.run()) {
      while (getTokenType == T.COMMA) advanceLexer()
    }
    if (getTokenType == T.RPAREN) advanceLexer()
    m.done(E.IMPORT_MEMBERS)
  }

  private def pImportMember = {
    pInParens(
      pWrapWith(E.IMPORT_MEMBER_VARSYM, pVarsym)
        .orElse(pWrapWith(E.IMPORT_MEMBER_CONSYM, pConsym))
    ).orElse(pWrapWith(E.IMPORT_MEMBER_CONID, pConid))
     .orElse(pWrapWith(E.IMPORT_MEMBER_VARID, pVarid))
  }

  private def pModuleDecl = parseIfToken(T.MODULETOKEN) { m =>
    pModuleName.run()
    pModuleExports.run()
    if (getTokenType == T.WHERE) advanceLexer()
    m.done(E.MODULE_DECL)
  }

  private def pModuleName = withMark { m =>
    if (pQConid.run()) {
      m.done(E.MODULE_NAME)
    } else {
      m.rollbackTo()
    }
  }

  private def pModuleExports = Parse {
    // TODO: Might be nicer to interpret "()" in the lexer as
    // LPAREN and RPAREN and interpret that sequence of tokens, when
    // encountered where we expect a Conid, as a Conid. This way we
    // don't have to have hacks like this.
    // Luckily, "( )" in the lexer _will_ be interpreted as
    // LPAREN PsiWhiteSpace RPAREN, so we don't need to special case that
    // here.
    if (getTokenText == "()") {
      withMark { m =>
        advanceLexer() // skip the "()" token
        m.done(E.MODULE_EXPORTS)
      }.run()
    } else if (getTokenType != T.LPAREN) {
      false
    } else {
      withMark { m =>
        advanceLexer() // skip seen LPAREN
        while (getTokenType != T.RPAREN) {
          // skip commas
          while (getTokenType == T.COMMA) advanceLexer()
          pModuleExport.run()
          // skip commas
          while (getTokenType == T.COMMA) advanceLexer()
        }
        m.done(E.MODULE_EXPORTS)
      }.run()
    }
  }

  private def pModuleExport = {
    pModuleExportModule
      .orElse(pModuleExportTyCon)
  }

  private def pModuleExportModule = parseIfToken(T.MODULETOKEN) { m =>
    if (!pModuleName.run()) error("Missing module name")
    m.done(E.MODULE_EXPORT_MODULE)
  }

  private def pModuleExportTyCon = withMark { m =>
    if (!pQTyCon.run()) {
      m.rollbackTo()
    } else {
      m.done(E.MODULE_EXPORT_TYCON)
    }
  }

  private def pTyCon = withMark { m =>
    if (pConid.run()) {
      m.done(E.TYCON_CONID)
    } else {
      m.rollbackTo()
    }
  }.orElse {
    withMark { m =>
      if (pConsym.run()) {
        m.done(E.TYCON_CONSYM)
      } else {
        m.rollbackTo()
      }
    }
  }

  private def pQTyCon = pQualified(pTyCon, E.QTYCON)

  private def pConid = pTokenAs(T.CONIDREGEXP, E.CONID)

  private def pQConid = pQualified(pConid, E.QCONID)

  private def pConsym = pTokenAs(T.CONSYMTOK, E.CONSYM)

  // TODO private def pQConsym = pQualified(pConsym, E.QCONSYM)

  private def pVarid = pTokenAs(T.VARIDREGEXP, E.VARID)

  // TODO private def pQVarid = pQualified(pVarid, E.QVARID)

  // NOTE: The HaskellParsingLexer does not emit 'VARSYMTOK'.
  private def pVarsym = pTokenAs(T.VARSYMTOKPLUS, E.VARSYM)

  // TODO private def pQVarsym = pQualified(pVarsym, E.QVARSYM)

  private def pQualified(p: Parse, e: E.HElementType) = withMark { m =>
    pQualifiedPrefix.run() // ok to ignore result
    if (p.run()) {
      m.done(e)
    } else {
      m.rollbackTo()
    }
  }

  private def pQualifiedPrefix = Parse {
    def hasMore = getTokenType == T.CONIDREGEXP && lookAhead(1) == T.PERIOD
    if (!hasMore) {
      false
    } else {
      withMark { m =>
        do {
          assert(pConid.run())
          assert(getTokenType == T.PERIOD)
          advanceLexer()
        } while (hasMore)
        m.done(E.QUALIFIED_PREFIX)
      }.run()
    }
  }

  private def pWrapWith(e: E.HElementType, p: Parse) = withMark { m =>
    if (p.run()) {
      m.done(e)
    } else {
      m.rollbackTo()
    }
  }

  private def pInParens(p: Parse) = Parse {
    if (getTokenType != T.LPAREN) {
      false
    } else {
      // Use a temporary marker so we can rollback the lexer state if we fail to parse.
      val m = mark()
      advanceLexer()
      if (p.run() && pToken(T.RPAREN).run()) {
        // Drop the temporary marker.
        m.drop()
        true
      } else {
        // Revert the lexer position such that we have not consumed the lparen.
        m.rollbackTo()
        false
      }
    }
  }

  //noinspection SameParameterValue
  private def pToken(t: HaskellTokenType) = Parse {
    if (getTokenType == t) {
      advanceLexer()
      true
    } else {
      false
    }
  }

  private def pTokenAs(t: HaskellTokenType, e: E.HElementType) = {
    parseIfToken(t) { m =>
      m.done(e)
    }
  }

  private def withMark(f: Marker => MarkResult): Parse = Parse {
    f(new Marker(mark())).consumed
  }

  private def parseIf(p: => Boolean)(f: Marker => MarkResult): Parse = Parse {
    if (!p) {
      false
    } else {
      withMark(f).run()
    }
  }

  private def parseIfToken(t: HaskellTokenType)(f: Marker => MarkResult): Parse = {
    parseIf(getTokenType == t) { m =>
      advanceLexer() // skip the checked token
      f(m)
    }
  }
}

object HaskellPsiBuilder {

  /** Parser returns 'true' if it consumed input or is considered successful. */
  final class Parse(val run: () => Boolean) extends AnyVal {
    def orElse(p: Parse): Parse = Parse { run() || p.run() }
  }

  object Parse {
    def apply(run: => Boolean): Parse = new Parse(() => run)
    val ok: Parse = Parse(true)
  }

  final class Marker(private val m: PsiBuilder.Marker) extends AnyVal {
    def done(t: E.HElementType): MarkResult = {
      m.done(t)
      MarkResult.Done
    }

    def rollbackTo(): MarkResult = {
      m.rollbackTo()
      MarkResult.Rollback
    }

    def error(message: String): MarkResult = {
      m.error(message)
      MarkResult.Error
    }

    def drop(): MarkResult = {
      m.drop()
      MarkResult.Drop
    }
  }

  sealed trait MarkResult {
    /** Indicates whether the 'Marker' action consumed input. */
    def consumed: Boolean
  }
  object MarkResult {
    case object Done extends MarkResult {
      override def consumed: Boolean = true
    }
    case object Rollback extends MarkResult {
      override def consumed: Boolean = false
    }
    case object Error extends MarkResult {
      override def consumed: Boolean = true
    }
    case object Drop extends MarkResult {
      override def consumed: Boolean = true
    }
  }
}
