package com.haskforce.tools.yesod.shakespeare.hamlet;

import com.haskforce.tools.yesod.shakespeare.hamlet.highlighting.HamletSyntaxHighlightingLexer;
import com.haskforce.tools.yesod.shakespeare.hamlet.psi.HamletFile;
import com.haskforce.tools.yesod.shakespeare.hamlet.psi.HamletTypes;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.NotNull;

public class HamletParserDefinition implements ParserDefinition {
    public static final IFileElementType FILE = new IFileElementType(Language.findInstance(HamletLanguage.class));

    @NotNull
    @Override
    public Lexer createLexer(Project project) {
        return new HamletSyntaxHighlightingLexer();
    }

    @Override
    public PsiParser createParser(Project project) {
        return new HamletParser();
    }

    @Override
    public IFileElementType getFileNodeType() {
        return FILE;
    }

    @NotNull
    @Override
    public TokenSet getWhitespaceTokens() {
        return TokenSet.create(TokenType.WHITE_SPACE);
    }

    @NotNull
    @Override
    public TokenSet getCommentTokens() {
        return TokenSet.create(HamletTypes.LINE_COMMENT);
    }

    @NotNull
    @Override
    public TokenSet getStringLiteralElements() {
        return TokenSet.create();
    }

    @NotNull
    @Override
    public PsiElement createElement(ASTNode node) {
        return new ASTWrapperPsiElement(node);
    }

    @Override
    public PsiFile createFile(FileViewProvider viewProvider) {
        return new HamletFile(viewProvider);
    }

    @Override
    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }
}
