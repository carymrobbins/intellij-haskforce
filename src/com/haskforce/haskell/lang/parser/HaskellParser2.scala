package com.haskforce.haskell.lang.parser

import com.haskforce.haskell.lang.lexer.HaskellParsingLexer2
import com.haskforce.haskell.lang.lexer.psi.LexerToken
import com.haskforce.haskell.lang.lexer.psi.Tokens._
import com.haskforce.haskell.lang.parser.psi.Elements._
import com.haskforce.haskell.lang.parser.psi.ParserElement
import com.haskforce.utils.parser.parsec.Psi
import com.haskforce.utils.parser.parsec.{PsiParsecTypedElements => PPTyped}
import com.haskforce.utils.parser.parsec.{PsiParsecUntypedElements => PPUntyped}
import com.intellij.lang.impl.PsiBuilderImpl
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.tree.IElementType
import scalaz.syntax.monad._

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

  private def atCol0(b: PsiBuilder): Boolean = {
    val offset = b.getCurrentOffset - 1
    if (offset < 0) return true
    val c = b.getOriginalText.charAt(offset)
    c == '\n' || c == '\r'
  }

  // Wow...this is...very...nice?
  val consumeUntilCol0: P0 =
    Psi(b =>
      while (!b.eof() && !atCol0(b)) {
        b.advanceLexer()
      }
    )

  val pUnknown: P0 =
    punlessM_(eof)(markStart(
      consumeUntilCol0
        *> markDone(UNKNOWN)
    ))

  val pModuleName: P0 = markStart(
    pif(maybeTokenOneOfAdvance(CONIDREGEXP))
      .pthen(markDone(MODULE_NAME))
      .pelse(markError("Missing module name"))
  )

  // Returns an Option[OpenBraceType]
  val pModule: Psi[Option[LexerToken]] =
    getTokenType >>= {
      case Some(MODULETOKEN) =>
        markStart(for {
          _ <- advanceLexer
          _ <- pModuleName
          _ <- expectTokenAdvance(WHERE)
          optOpenBrace <- getTokenType >>= [Option[LexerToken]] {
            case ot@(None | Some(LBRACE | WHITESPACELBRACETOK)) =>
              advanceLexer *> pure(ot)
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

  val pImportModule: P0 = markStart(
    pif(maybeTokenOneOfAdvance(CONIDREGEXP))
      .pthen(markDone(IMPORT_MODULE))
      .pelse(markError("Missing import module"))
  )

  def pImportName(node: ParserElement): P0 =
    getTokenType.map(_.contains(LPAREN)).flatMap(hasParen =>
         advanceLexer.whenM(hasParen)
      >> withTokenType(t =>
           pif(pure(List(CONIDREGEXP, VARIDREGEXP).contains(t))).pthen(
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
      if (mt.contains(LPAREN)) advanceLexer
      else if (mt.contains(COMMA)) advanceLexer >> pImportName(node) >> loop
      else error("Expected close parent or comma in explicit name import")
    )

    getTokenType.flatMap(mt =>
      if (mt.contains(LPAREN)) advanceLexer
      else pImportName(node) >> loop
    )
  }

  val pImport: P0 = markStart(
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
    >> markDone(IMPORT_MODULE)
  )

  val pImports: P0 = {
    lazy val loop: P0 = (
      pImport
      >> whenTokenIs_(_ == SEMICOLON)(advanceLexer)
      >> whenTokenIs_(_ == IMPORT)(loop)
    )

    whenTokenIs_(_ == IMPORT)(markStart(
         loop
      >> markDone(IMPORT_LIST)
    ))
  }

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

  val pForeignImport: Psi[Boolean] =
    inspect(b =>
      lookAheadMany(2) >>= (ts =>
        pwhen(ts == List(FOREIGN, IMPORT))(
          markStart(
            times(advanceLexer, 2)
              *> many_(VARIDREGEXP)
              *> (pStringLit >>= (if (_) expectTokenAdvance(VARIDREGEXP) else rUnit))
              *> expectTokenAdvance(DOUBLECOLON)
              *> pType
              *> markDone(FOREIGN_IMPORT)
          )
        )
      )
    )

  val pTopDecl: Psi[Boolean] =
    pany(
      pForeignImport
    )

  val body: P0 = pforever(pTopDecl >>= (matched =>
    if (matched) rUnit else pUnknown
  ))

  val file: P0 = (
    pModule
    >> pImports
    >> body
  )
}
