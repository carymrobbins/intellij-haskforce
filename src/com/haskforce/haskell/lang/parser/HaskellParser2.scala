package com.haskforce.haskell.lang.parser

import com.haskforce.haskell.lang.lexer.psi.LexerToken
import com.haskforce.haskell.lang.lexer.psi.Tokens._
import com.haskforce.haskell.lang.parser.psi.Elements._
import com.haskforce.haskell.lang.parser.psi.ParserElement
import com.haskforce.utils.parser.parsec.{Psi, PsiParsecTypedElements => PPTyped, PsiParsecUntypedElements => PPUntyped}
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType
import scalaz.syntax.monad._

import scala.annotation.tailrec

final class HaskellParser2 extends PsiParser {

  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    PPUntyped.markStart(
      HaskellParser2Parsec.file
        >> HaskellParser2Parsec.pUnknown
        >> PPUntyped.markDone(root)
    ).run(builder)
    builder.getTreeBuilt
  }
}

object HaskellParser2Parsec extends PPTyped[LexerToken, ParserElement] {

  type P0 = Psi[Unit]

  // Consume until we encounter a token at column 1. This will allow us
  // to make the parser more recoverable and continue parsing even with
  // bad input.
  val consumeUntilLineStart: P0 =
    Psi(b => while (!b.eof() && !atLineStart(b)) b.advanceLexer())

  val pUnknown: P0 =
    punlessM_(eof)(markStart(
      advanceLexer
        *> consumeUntilLineStart
        *> markDone(UNKNOWN)
    ))

  val pModuleName: P0 = markStart(
    pif(maybeTokenOneOfAdvance(CONIDREGEXP))
      .pthen(markDone(MODULE_NAME))
      .pelse(markError("Missing module name"))
  )

  // Returns an Option[OpenBraceType]
  val pModuleDecl: Psi[Option[LexerToken]] =
    getTokenType >>= {
      case Some(MODULETOKEN) =>
        markStart(for {
          _ <- advanceLexer
          _ <- pModuleName
          _ <- expectTokenAdvance(WHERE)
          optOpenBrace <- getTokenType >>= [Option[LexerToken]] {
            case ot@(None | Some(LBRACE | WHITESPACELBRACETOK)) =>
              advanceLexer *> ppure(ot)
            case Some(t) =>
              error(s"Unexpected token after module decl: $t") *> rNone
          }
          r <- markDoneWith(MODULE_DECL, optOpenBrace)
        } yield r)
      case ot =>
        error(
          s"Expected 'module' but got: " + ot.map(_.toString).getOrElse("end of input")
        ) *> rNone
    }

  val pQConId: Psi[Boolean] =
    whenTokenIs(CONIDREGEXP)(
      markStart(
        advanceLexer
          *> pwhile(lookAheadManyIs(PERIOD, CONIDREGEXP))(
               times(advanceLexer, 2)
             )
          *> markDone(QCONID)
      )
    )

  val pImportModule: P0 = markStart(
    pif(pQConId)
      .pthen(markDone(IMPORT_MODULE))
      .pelse(markError("Missing import module"))
  )

  def pImportName(node: ParserElement): P0 =
    getTokenType.map(_.contains(LPAREN)).flatMap(hasParen =>
         advanceLexer.whenM(hasParen)
      >> withTokenType(t =>
           pif(ppure(List(CONIDREGEXP, VARIDREGEXP).contains(t))).pthen(
                  markStart(advanceLexer >> markDone(node))
               >> pImportCtorsOrMethods(node)
             ).pelse(error("Invalid explicit import; expected id, constructor, or symbol"))
           >> expectTokenAdvance(RPAREN).whenM(hasParen)
        )
    )

  def pImportCtorsOrMethods(node: ParserElement): P0 = {
    lazy val loop: P0 = (
      pImportName(node)
      >> whenTokenIs_(_ == COMMA)(advanceLexer >> loop)
    )
    whenTokenIs_(_ == LPAREN)(
      advanceLexer
        >> withTokenType(t =>
          if (t == DOUBLEPERIOD) markStart(advanceLexer >> markDone(node))
          else loop
        )
        >> expectTokenAdvance(RPAREN)
    )
  }

  def pImportNames(node: ParserElement): P0 = {

    lazy val loop: P0 = getTokenType.flatMap(mt =>
      if (mt.contains(RPAREN)) advanceLexer
      else if (mt.contains(COMMA)) advanceLexer >> pImportName(node) >> loop
      else error("Expected close paren or comma in explicit name import")
    )

    getTokenType.flatMap(mt =>
      if (mt.contains(LPAREN)) advanceLexer
      else pImportName(node) >> loop
    )
  }

  val pImportStmt: P0 = markStart(
       advanceLexer
    >> whenTokenIs(_ == QUALIFIED)(advanceLexer)
    >> pImportModule
    >> whenTokenIs(_ == AS)(
            advanceLexer
         >> markStart(
                 expectTokenAdvance(CONIDREGEXP)
              >> markDone(IMPORT_ALIAS)
            )
      )
    >> whenTokenIs(_ == LPAREN)(
            advanceLexer
         >> markStart(
              // Also handles the RPAREN
              pImportNames(IMPORT_EXPLICIT)
           >> markDone(IMPORT_EXPLICITS)
         )
       )
    >> whenTokenIs(_ == HIDING)(
            advanceLexer
         >> expectTokenAdvance(LPAREN)
         >> markStart(
           pImportNames(IMPORT_HIDDEN)
           >> markDone(IMPORT_HIDDENS)
         )
       )
    >> markDone(IMPORT_STMT)
  )

  val pImportList: P0 =
    whenTokenIs_(_ == IMPORT)(markStart(
      pwhile(isTokenType(IMPORT))(
        pImportStmt *> maybeTokenAdvance_(WHITESPACESEMITOK)
      ) *> markDone(IMPORT_LIST)
    ))

  val pType: P0 = // TODO
    markStart(pUnknown *> markDone(TYPE_EXPR))

  val pStringLit: Psi[Boolean] = whenTokenIs(DOUBLEQUOTE)(
    markStart(
      advanceLexer
        *> expectTokenAdvance(STRINGTOKEN)
        *> expectTokenAdvance(DOUBLEQUOTE)
        *> markDone(STRING_LIT)
    )
  )

  val pForeignDecl: Psi[Boolean] =
    whenTokenIs(FOREIGN)(
      markStart(
        advanceLexer *> getTokenText.flatMap { optForeignType =>
          val optResultNode = optForeignType match {
            case Some("import") => Some(FOREIGN_IMPORT)
            case Some("export") => Some(FOREIGN_EXPORT)
            case _              => None
          }
          optResultNode match {
            case None =>
              (consumeUntilLineStart
                *> markError("Expected import or export after 'foreign'"))
            case Some(resultNode) =>
              (advanceLexer
                *> many_(VARIDREGEXP)
                *> (pStringLit >>= (if (_) expectTokenAdvance(VARIDREGEXP) else rUnit))
                *> expectTokenAdvance(DOUBLECOLON)
                *> pType
                *> markDone(resultNode))
          }
        }
      )
    )

//  def pForeignDecl(importOrExport: LexerToken, node: ParserElement): Psi[Boolean] =
//    lookAheadMany(2) >>= (ts =>
//      pwhen(ts == List(FOREIGN, importOrExport))(
//        markStart(
//          times(advanceLexer, 2)
//            *> many_(VARIDREGEXP)
//            *> (pStringLit >>= (if (_) expectTokenAdvance(VARIDREGEXP) else rUnit))
//            *> expectTokenAdvance(DOUBLECOLON)
//            *> pType
//            *> markDone(node)
//        )
//      )
//    )
//
//  val pForeignImport: Psi[Boolean] = pForeignDecl(IMPORT, FOREIGN_IMPORT)
//
//  val pForeignExport: Psi[Boolean] = pForeignDecl(EXPORT, FOREIGN_EXPORT)

  val pTopDecl: Psi[Boolean] =
    pany(
      pForeignDecl
    )

  val body: P0 = pforever(pTopDecl >>= (matched =>
    if (matched) rUnit else pUnknown
  ))

  val file: P0 = (
    pModuleDecl
    >> pImportList
    >> body
  )

  // I'm surprised this isn't easier to get from the lexer.
  // I've tried (essentially) b.getLexer.getFlex.yycolumn and it
  // seems the lexer is done and yycolumn never changes (it's just stuck
  // at whatever its last state was at the end of lexing).
  private def getColumn(s: CharSequence, offset: Int): Int = {
    @tailrec
    def loop(col: Int, index: Int): Int = {
      if (index == -1) return 1
      val c = s.charAt(index)
      if (c == '\n' || c == '\r') return col
      loop(col + 1, index - 1)
    }
    loop(0, offset)
  }

  private def atLineStart(b: PsiBuilder): Boolean =
    atLineStart(b.getOriginalText, b.getCurrentOffset)

  // Slightly more optimized way of determining if we're at the start of a line,
  // i.e. either on a newline or char 1 of the line.
  private def atLineStart(s: CharSequence, offset: Int): Boolean = {
    // Correct an offset which has exceeded the length of the input string.
    val i = Math.min(offset, s.length - 1)
    if (i == 0) return true
    val c1 = s.charAt(i)
    if (c1 == '\n' || c1 == '\r') return true
    val c0 = s.charAt(i - 1)
    if (c0 == '\n' || c0 == '\r') return true
    false
  }
}
