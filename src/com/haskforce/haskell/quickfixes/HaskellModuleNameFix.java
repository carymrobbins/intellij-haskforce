package com.haskforce.quickfixes;

import com.haskforce.haskell.psi.HaskellConid;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.RefactoringFactory;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;


/**
 * Renames a module to match the filename.
 */
public class HaskellModuleNameFix extends BaseIntentionAction implements LocalQuickFix {
    private final String myTargetName;
    private final HaskellConid myPsiNode;

    public HaskellModuleNameFix(@NotNull HaskellConid e,
                                @NotNull String newName) {
        myTargetName = newName;
        myPsiNode = e;
    }

    @Override
    @NotNull
    public String getText() {
        return "Rename Module";
    }

    @Override
    @NotNull
    public String getName() {
        return getText();
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Haskell";
    }

    /**
     * Checks whether this intention is available at a caret offset in file.
     */
    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        return true;
    }

    /**
     * Called when user invokes intention.
     */
    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
        RefactoringFactory.getInstance(project).createRename(myPsiNode, myTargetName).doRefactoring(UsageInfo.EMPTY_ARRAY);
    }

    @Override
    public boolean startInWriteAction() {
        return true;
    }

    @Override
    public void applyFix(@NotNull final Project project, @NotNull ProblemDescriptor descriptor) {
        final PsiFile file = descriptor.getPsiElement().getContainingFile();
        if (isAvailable(project, null, file)) {
            new WriteCommandAction(project) {
                @Override
                protected void run(@NotNull Result result) throws Throwable {
                    invoke(project, null, file);
                }
            }.execute();
        }
    }
}
