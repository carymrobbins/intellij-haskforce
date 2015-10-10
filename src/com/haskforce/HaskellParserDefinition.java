package com.haskforce;

import com.haskforce.parsing.HaskellParsingLexer;
import com.haskforce.psi.HaskellParserWrapper;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.stubs.types.HaskellFileStubElementType;
import com.intellij.lang.ASTNode;
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
import com.haskforce.psi.HaskellFile;
import org.jetbrains.annotations.NotNull;

/**
 * Main entry point from plugin for parsing. Returns parser, lexer and other
 * things useful for parsing.
 */
public class HaskellParserDefinition implements ParserDefinition {
    public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
    public static final TokenSet COMMENTS = TokenSet.create(
            HaskellTypes.COMMENT, HaskellTypes.HADDOCK,
            HaskellTypes.COMMENTTEXT, HaskellTypes.OPENCOM, HaskellTypes.CLOSECOM,
            // Interpret C preprocessor directives as comments.
            HaskellTypes.CPPIF, HaskellTypes.CPPELSE, HaskellTypes.CPPENDIF,
            HaskellTypes.CPPIFDEF, HaskellTypes.CPPELIF,
            HaskellTypes.CPPDEFINE, HaskellTypes.CPPUNDEF, HaskellTypes.CPPLINE,
            HaskellTypes.CPPPRAGMA
            );
    public static final TokenSet STRINGS = TokenSet.create(HaskellTypes.STRINGTOKEN);

//    public static final IFileElementType FILE = new IFileElementType(Language.<HaskellLanguage>findInstance(HaskellLanguage.class));

    @NotNull
    @Override
    public Lexer createLexer(Project project) {
        return new HaskellParsingLexer();
    }

    /**
     * These tokens are filtered out by the PsiBuilder before they reach the
     * parser.
     */
    @NotNull
    public TokenSet getWhitespaceTokens() {
        return WHITE_SPACES;
    }

    /**
     * These tokens are filtered out by the PsiBuilder before they reach the
     * parser. They are also searched for TODO items.
     */
    @NotNull
    public TokenSet getCommentTokens() {
        return COMMENTS;
    }

    @NotNull
    public TokenSet getStringLiteralElements() {
        return STRINGS;
    }

    @NotNull
    @Override
    public PsiParser createParser(final Project project) {
        return new HaskellParserWrapper();
    }

    /**
     * Use a the file stub element type so the contents of these files will be indexed.
     */
    @Override
    public IFileElementType getFileNodeType() {
        return HaskellFileStubElementType.INSTANCE;
    }

    public PsiFile createFile(FileViewProvider viewProvider) {
        return new HaskellFile(viewProvider);
    }

    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }

    /**
     * Gets called when PsiBuilder.Marker.done(node) is called by the parser.
     */
    @NotNull
    public PsiElement createElement(ASTNode node) {
        return HaskellTypes.Factory.createElement(node);
    }
}
