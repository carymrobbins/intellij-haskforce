package com.haskforce.codeInsight;

import com.haskforce.HaskellLanguage;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionProvider;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.editor.Editor;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.HashSet;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;

/**
 * Fills the list of completions available on ctrl-space.
 */
public class HaskellCompletionContributor extends CompletionContributor {
    private static final HashSet<String> keywords = new HashSet<String>(
            Arrays.asList(new String[]{"as", "case", "class", "data", "default"
                    , "deriving", "do", "else", "export", "forall", "foreign"
                    , "hiding", "if", "import", "in", "infix", "infixl", "infixr"
                    , "instance", "let", "module", "newtype", "of", "qualified"
                    , "safe", "then", "type", "where"}));

    public HaskellCompletionContributor() {
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet resultSet) {
                        for (String word : keywords) {
                            resultSet.addElement(LookupElementBuilder.create(word));
                        }
                    }
                }
        );
    }

    /**
     * Adjust the error message when no lookup is found.
     */
    @Nullable
    @Override
    public String handleEmptyLookup(@NotNull CompletionParameters parameters, final Editor editor) {
        return "HaskForce: no completion found.";
    }
}