package com.haskforce.haskell.features.intentions;

import com.haskforce.highlighting.annotation.external.GhcMod.*;
import com.haskforce.utils.FileUtil;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

public class RemoveForall extends BaseIntentionAction {
    public final Problem problem;

    public RemoveForall(Problem problem) {
        this.problem = problem;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Remove forall from type signature";
    }

    @NotNull
    @Override
    public String getText() {
        return "Remove forall from type signature";
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        // TODO: Add a setting for this.
        return true;
    }

    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
        FileUtil.updateFileText(project, file, new Function<String, String>() {
            public String fun(String text) {
                final int offset = StringUtil.lineColToOffset(text, problem.startLine - 1, problem.startColumn - 1);
                // Doing offset + 1 to catch the dot '.' after the forall in the type signature.
                final String beginning = text.substring(0, offset + 1);
                final String ending = text.substring(offset + 1);
                final Pattern pattern = Pattern.compile("::\\s?forall\\s[^\\.]+\\.\\s*$");
                final String newBeginning = pattern.matcher(beginning).replaceFirst("::");
                return newBeginning + ending;
            }
        });
    }
}
