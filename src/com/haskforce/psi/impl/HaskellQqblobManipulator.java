package com.haskforce.psi.impl;

import com.haskforce.psi.HaskellQqblob;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public class HaskellQqblobManipulator extends AbstractElementManipulator<HaskellQqblob> {
    @Override
    public HaskellQqblob handleContentChange(@NotNull HaskellQqblob psi, @NotNull TextRange range, String newContent) throws IncorrectOperationException {
        final String oldText = psi.getText();
        final String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());
        return psi.updateText(newText);
    }
}
