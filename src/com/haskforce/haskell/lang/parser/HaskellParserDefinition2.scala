package com.haskforce.haskell.lang.parser

import com.haskforce.haskell.lang.lexer.HaskellParsingLexer2
import com.haskforce.haskell.lang.lexer.psi.Tokens
import com.haskforce.haskell.lang.parser.psi.{ElementFactory, Elements}
import com.intellij.lang.{ASTNode, ParserDefinition, PsiParser}
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.{FileViewProvider, PsiElement, PsiFile}
import com.intellij.psi.tree.{IFileElementType, TokenSet}

class HaskellParserDefinition2 extends ParserDefinition {

  override def createLexer(project: Project): Lexer =
    new HaskellParsingLexer2()

  override def createParser(project: Project): PsiParser =
    new HaskellParser2()

  override def getFileNodeType: IFileElementType =
    Elements.HASKELL_FILE

  override val getCommentTokens: TokenSet =
    TokenSet.create(
      Tokens.COMMENT, Tokens.COMMENTTEXT,
      Tokens.HADDOCK,
      Tokens.OPENCOM, Tokens.CLOSECOM,
      Tokens.CPPIF, Tokens.CPPELSE, Tokens.CPPENDIF,
      Tokens.CPPIFDEF, Tokens.CPPELIF,
      Tokens.CPPDEFINE, Tokens.CPPUNDEF, Tokens.CPPLINE,
      Tokens.CPPPRAGMA
    )

  override val getStringLiteralElements: TokenSet =
      TokenSet.EMPTY

  override def createElement(node: ASTNode): PsiElement =
    ElementFactory.createElement(node)

  override def createFile(viewProvider: FileViewProvider): PsiFile =
    new HaskellFile2(viewProvider)

  override def spaceExistanceTypeBetweenTokens(left: ASTNode, right: ASTNode): ParserDefinition.SpaceRequirements =
    ParserDefinition.SpaceRequirements.MAY

  override def getWhitespaceTokens: TokenSet =
    TokenSet.create(Tokens.WHITE)
}
