package com.haskforce;

import com.haskforce.haskell.lang.parser.HaskellParser2020;
import com.haskforce.parser.HaskellParser;
import com.haskforce.parsing.HaskellParsingLexer;
import com.haskforce.psi.HaskellParserWrapper;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.stubs.types.HaskellFileStubElementType;
import com.haskforce.utils.parser.SimplePsiParser;
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

import java.util.function.Function;

/**
 * Main entry point from plugin for parsing. Returns parser, lexer and other
 * things useful for parsing.
 */
public class HaskellParserDefinition implements ParserDefinition {

    private static final Mode SYSTEM_PARSER_MODE;
    static {
        final String s = System.getProperty("com.haskforce.parser", "default").trim().toLowerCase();
        switch (s) {
            case "noop":
                SYSTEM_PARSER_MODE = Mode.NOOP;
                break;
            case "unwrapped":
                SYSTEM_PARSER_MODE = Mode.UNWRAPPED;
                break;
            case "2020":
                SYSTEM_PARSER_MODE = Mode.PARSER2020;
                break;
            default:
                SYSTEM_PARSER_MODE = Mode.DEFAULT;
                break;
        }
    }

    public HaskellParserDefinition() {
      this(SYSTEM_PARSER_MODE);
    }

    public HaskellParserDefinition(Mode mode) {
        this.mode = mode;
    }

    public enum Mode { DEFAULT, NOOP, UNWRAPPED, PARSER2020 }

    private final Mode mode;

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
      switch (mode) {
          case NOOP: return new SimplePsiParser();
          case UNWRAPPED: return new HaskellParser();
          case PARSER2020: return new HaskellParser2020();
          default: return new HaskellParserWrapper();
      }
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

    @Override
    public SpaceRequirements spaceExistenceTypeBetweenTokens(ASTNode left, ASTNode right) {
        return SpaceRequirements.MAY;
    }

    /**
     * Gets called when PsiBuilder.Marker.done(node) is called by the parser.
     */
    @NotNull
    @Override
    public PsiElement createElement(ASTNode node) {
        if (mode == Mode.PARSER2020) {
            return com.haskforce.haskell.lang.parser.gen.Factory$.MODULE$.createElement(node);
        }
        return HaskellTypes.Factory.createElement(node);
    }
}
