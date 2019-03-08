package com.haskforce.haskell.lang.parser

import com.haskforce.haskell.lang.lexer.psi.LexerToken
import com.haskforce.haskell.lang.lexer.psi.Tokens._
import com.haskforce.haskell.lang.parser.psi.Elements._
import com.haskforce.haskell.lang.parser.psi.ParserElement
import com.haskforce.utils.parser.parsec.{Psi, PsiParsecTypedElements}
import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType
import scalaz.syntax.monad._

final class HaskellParser2 extends PsiParser {

  override def parse(root: IElementType, builder: PsiBuilder): ASTNode =
    HaskellParser2Parsec.runParser(root.asInstanceOf[ParserElement], builder)
}

object HaskellParser2Parsec
    extends PsiParsecTypedElements[LexerToken, ParserElement] {

  def runParser(root: ParserElement, builder: PsiBuilder): ASTNode = {
    markStart(top *> consumeUntilEOF *> markDone(root)).run(builder)
    builder.getTreeBuilt
  }

  type P0 = Psi[Unit]

  val pModuleName: P0 = markStart(
    pif(maybeTokenOneOfAdvance(CONIDREGEXP))
      .pthen(markDone(MODULE_NAME))
      .pelse(markError("Missing module name"))
  )

  val pModule: P0 =
    whenTokenIs(_ == MODULETOKEN)(markStart(
         advanceLexer
      *> pModuleName
      *> expectTokenAdvance(WHERE)
      *> expectTokenOneOfAdvance(LBRACE /*, VIRTUAL_LBRACE*/)
      *> markDone(MODULE_DECL)
    ))

  val pImportModule: P0 = markStart(
    pif(maybeTokenOneOfAdvance(CONIDREGEXP))
      .pthen(markDone(IMPORT_MODULE))
      .pelse(markError("Missing import module"))
  )

  def pImportName(node: IElementType): P0 =
    getTokenType.map(_.contains(LPAREN)).flatMap(hasParen =>
         advanceLexer.whenM(hasParen)
      *> withTokenType(t =>
           pif(pure(List(CONID, VARID, CONSYM, VARSYM).contains(t))).pthen(
                  markStart(advanceLexer *> markDone(node))
               *> pImportCtorsOrMethods(node)
             ).pelse(error("Invalid explicit import; expected id, constructor, or symbol"))
           *> expectTokenAdvance(RPAREN).whenM(hasParen)
        )
    )

  def pImportCtorsOrMethods(node: IElementType): P0 = ???

  def pImportNames(node: IElementType): P0 = {

    lazy val loop = getTokenType.flatMap(mt =>
      if (mt.contains(LPAREN)) advanceLexer
      else if (mt.contains(COMMA)) advanceLexer *> pImportName(node) *> loop
      else error("Expected close parent or comma in explicit name import")
    )

    getTokenType.flatMap(mt =>
      if (mt.contains(LPAREN)) advanceLexer
      else pImportName(node) *> loop
    )
  }

  val pImport: P0 = markStart(
       advanceLexer
    *> whenTokenIs(_ == QUALIFIED)(advanceLexer)
    *> pImportModule
    *> whenTokenIs(_ == AS)(
            advanceLexer
         *> markStart(
                 expectTokenAdvance(CONID)
              *> markDone(???/*IMPORT_ALIAS*/)
            )
      )
    *> whenTokenIs(_ == LPAREN)(
            advanceLexer
         *> markStart(
              // Also handles the RPAREN
              pImportNames(??? /*IMPORT_EXPLICIT*/)
           *> markDone(???/*IMPORT_EXPLICITS*/)
         )
       )
    *> whenTokenIs(_ == HIDING)(
            advanceLexer
         *> expectTokenAdvance(LPAREN)
         *> markStart(
           pImportNames(???/*IMPORT_HIDDEN*/)
           *> markDone(???/*IMPORT_HIDDENS*/)
         )
       )
    *> markDone(IMPORTT)
  )

  val pImports: P0 = {
    lazy val loop = (
      pImport
      *> whenTokenIs(_ == SEMICOLON)(advanceLexer)
      *> whenTokenIs(_ == IMPORT)(loop)
    )

    whenTokenIs(_ == IMPORT)(markStart(
         loop
      *> markDone(???/*IMPORTS*/)
    ))
  }

  val pUnknown: P0 =
    pwhenM(eof)(markStart(
      consumeUntilEOF
      *> markDone(???/*UNKNOWN*/)
    ))

  val top: P0 = (
    pModule
    *> pImports
    *> pUnknown
  )
}
