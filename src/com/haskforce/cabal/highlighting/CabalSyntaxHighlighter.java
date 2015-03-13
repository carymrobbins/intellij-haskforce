package com.haskforce.cabal.highlighting;

import com.haskforce.cabal.psi.CabalParsingLexer;
import com.haskforce.cabal.psi.CabalTypes;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class CabalSyntaxHighlighter extends SyntaxHighlighterBase {
    public static final TextAttributesKey COLON = DefaultLanguageHighlighterColors.OPERATION_SIGN;
    public static final TextAttributesKey KEY = DefaultLanguageHighlighterColors.INSTANCE_FIELD;
    public static final TextAttributesKey COMMENT =  DefaultLanguageHighlighterColors.LINE_COMMENT;
    public static final TextAttributesKey CONFIG = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey CONDITIONAL = DefaultLanguageHighlighterColors.INSTANCE_METHOD;

    private static final Map<IElementType, TextAttributesKey> keys;

    /**
     * Helper to point multiple token types to a single color.
     */
    private static void keysPutEach(Iterable<IElementType> tokenTypes, TextAttributesKey value) {
        for (IElementType tokenType : tokenTypes) {
            keys.put(tokenType, value);
        }
    }

    static {
        keys = new HashMap<IElementType, TextAttributesKey>(0);
        keys.put(CabalTypes.COLON, COLON);
        keys.put(CabalTypes.COMMENT, COMMENT);
        keysPutEach(Arrays.asList(CabalTypes.IF, CabalTypes.ELSE), CONDITIONAL);
        keysPutEach(Arrays.asList(CabalTypes.LIBRARY, CabalTypes.EXECUTABLE,
                CabalTypes.TEST_SUITE, CabalTypes.FLAG, CabalTypes.BENCHMARK,
                CabalTypes.SOURCEREPOSITORYKEY), CONFIG);
        keysPutEach(Arrays.asList(
                CabalTypes.ADDRESS,
                CabalTypes.AUTHORKEY,
                CabalTypes.BUGREPORTSKEY,
                CabalTypes.BUILDABLEKEY,
                CabalTypes.BUILDDEPENDSKEY,
                CabalTypes.BUILDTOOLSKEY,
                CabalTypes.BUILDTYPEKEY,
                CabalTypes.CABALVERSIONKEY,
                CabalTypes.CATEGORYKEY,
                CabalTypes.CCOPTIONSKEY,
                CabalTypes.CONDTIONAL_KEY,
                CabalTypes.COPYRIGHTKEY,
                CabalTypes.CPPOPTIONSKEY,
                CabalTypes.DATADIRKEY,
                CabalTypes.DATAFILESKEY,
                CabalTypes.DEFAULTLANGUAGEKEY,
                CabalTypes.DEFAULTFLAGVALUEKEY,
                CabalTypes.DESCRIPTIONKEY,
                CabalTypes.EXPOSEDKEY,
                CabalTypes.EXPOSEDMODULESKEY,
                CabalTypes.EXTENSIONSKEY,
                CabalTypes.EXTRADOCFILESKEY,
                CabalTypes.EXTRAGHCILIBRARIESKEY,
                CabalTypes.EXTRALIBDIRSKEY,
                CabalTypes.EXTRALIBRARIESKEY,
                CabalTypes.EXTRASOURCEFILESKEY,
                CabalTypes.EXTRATMPFILESKEY,
                CabalTypes.FRAMEWORKSKEY,
                CabalTypes.GHCOPTIONSKEY,
                CabalTypes.GHCPROFOPTIONSKEY,
                CabalTypes.GHCSHAREDOPTIONSKEY,
                CabalTypes.HOMEPAGEKEY,
                CabalTypes.HSSOURCEDIRSKEY,
                CabalTypes.INCLUDEDIRSKEY,
                CabalTypes.INCLUDESKEY,
                CabalTypes.INSTALLINCLUDESKEY,
                CabalTypes.JSSOURCESKEY,
                CabalTypes.LDOPTIONSKEY,
                CabalTypes.LICENSEFILEKEY,
                CabalTypes.LICENSEFILESKEY,
                CabalTypes.LICENSEKEY,
                CabalTypes.LOCATIONKEY,
                CabalTypes.MAINISKEY,
                CabalTypes.MAINTAINERKEY,
                CabalTypes.MANUALKEY,
                CabalTypes.MODULEKEY,
                CabalTypes.NAMEKEY,
                CabalTypes.OTHEREXTENSIONSKEY,
                CabalTypes.OTHERMODULESKEY,
                CabalTypes.PACKAGEKEY,
                CabalTypes.PKGCONFIGDEPENDSKEY,
                CabalTypes.SOURCEREPOSITORYKEY,
                CabalTypes.STABILITYKEY,
                CabalTypes.SUBDIRKEY,
                CabalTypes.SYNOPSISKEY,
                CabalTypes.TAGKEY,
                CabalTypes.TESTEDWITHKEY,
                CabalTypes.TESTMODULEKEY,
                CabalTypes.TYPEKEY,
                CabalTypes.VERSIONKEY
        ), KEY);
    }

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new CabalParsingLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(keys.get(tokenType), EMPTY);
    }
}
