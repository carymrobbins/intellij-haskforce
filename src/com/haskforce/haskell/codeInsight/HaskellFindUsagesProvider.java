package com.haskforce.haskell.codeInsight;

import com.haskforce.haskell.HaskellParserDefinition;
import com.haskforce.haskell.highlighting.HaskellSyntaxHighlightingLexer;
import com.haskforce.haskell.psi.HaskellTypes;
import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.lang.cacheBuilder.WordsScanner;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.ElementDescriptionUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.tree.TokenSet;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * "Find usages" in right-click on top of an identifier.
 */
public class HaskellFindUsagesProvider implements FindUsagesProvider {
    @SuppressWarnings("UnusedDeclaration")
    private final static Logger LOG = Logger.getInstance(HaskellFindUsagesProvider.class);
    // Second parameter is nodes that are PsiNamedElements in practice.
    private final static WordsScanner SCANNER =
            new DefaultWordsScanner(new HaskellSyntaxHighlightingLexer(),
                    TokenSet.create(HaskellTypes.VARIDREGEXP,
                            HaskellTypes.CONIDREGEXP),
                    HaskellParserDefinition.COMMENTS, HaskellParserDefinition.STRINGS);
    @Nullable
    @Override
    public WordsScanner getWordsScanner() {
        return SCANNER;
    }

    @Override
    public boolean canFindUsagesFor(@NotNull PsiElement psiElement) {
        return psiElement instanceof PsiNamedElement;
    }

    @Nullable
    @Override
    public String getHelpId(@NotNull PsiElement psiElement) {
        // TODO: Use HelpID after 13.1.
        return "reference.dialogs.findUsages.other";
    }

    @NotNull
    @Override
    public String getType(@NotNull PsiElement element) {
        return ElementDescriptionUtil.getElementDescription(element, UsageViewTypeLocation.INSTANCE);
    }

    @NotNull
    @Override
    public String getDescriptiveName(@NotNull PsiElement element) {
        return ElementDescriptionUtil.getElementDescription(element, UsageViewLongNameLocation.INSTANCE);
    }

    @NotNull
    @Override
    public String getNodeText(@NotNull PsiElement element, boolean useFullName) {
        return ElementDescriptionUtil.getElementDescription(element, UsageViewNodeTextLocation.INSTANCE);
    }
}
