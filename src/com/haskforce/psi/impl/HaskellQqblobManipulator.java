package com.haskforce.psi.impl;

import com.haskforce.psi.HaskellQqblob;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

public class HaskellQqblobManipulator extends AbstractElementManipulator<HaskellQqblob> {
    @Override
    public HaskellQqblob handleContentChange(@NotNull HaskellQqblob psi, @NotNull TextRange range, String newContent) throws IncorrectOperationException {
        final String oldText = psi.getText();
        final String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());
        return psi.updateText(newText);
    }

    public static final Pattern LEADING_SLASH_ESCAPE_REGEX = Pattern.compile("\\\\\\s.*");

    @NotNull
    public TextRange getRangeInElement(@NotNull HaskellQqblob psi) {
        return pairToTextRange(getRangeForText(psi.getText()));
    }

    private static Pair<Integer, Integer> getRangeForText(String text) {
        final int start = LEADING_SLASH_ESCAPE_REGEX.matcher(text).matches() ? 2 : 1;
        final int end = text.length() - start;
        return new Pair(start, end);
    }

    private static TextRange pairToTextRange(Pair<Integer, Integer> pair) {
        final int start = Math.max(pair.first, 0);
        final int end = Math.max(pair.second, start);
        return TextRange.from(start, end);
    }
}
