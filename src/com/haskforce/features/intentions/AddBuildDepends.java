package com.haskforce.features.intentions;


import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public class AddBuildDepends extends BaseIntentionAction {

    public final String packageName;

    public AddBuildDepends(String packageName) {
        this.packageName = packageName;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Add build depends";
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile psiFile) {
        return true;
    }

    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile psiFile) throws IncorrectOperationException {
        //TODO
    }
}
