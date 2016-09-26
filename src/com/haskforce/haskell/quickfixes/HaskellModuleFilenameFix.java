package com.haskforce.haskell.quickfixes;

import com.intellij.codeInsight.CodeInsightBundle;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ex.MessagesEx;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

/**
 * Renames a file to match the module name.
 */
public class HaskellModuleFilenameFix extends BaseIntentionAction implements LocalQuickFix {
    private final String myTargetName;

    public HaskellModuleFilenameFix(@NotNull String newName) {
        myTargetName = newName;
    }

    @Override
    @NotNull
    public String getText() {
        return CodeInsightBundle.message("rename.file.fix");
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
        VirtualFile vFile = file.getVirtualFile();
        Document document = PsiDocumentManager.getInstance(project).getDocument(file);
        if (document == null) return;

        FileDocumentManager.getInstance().saveDocument(document);
        try {
            vFile.rename(file.getManager(), myTargetName);
        } catch (IOException e) {
            MessagesEx.error(project, e.getMessage()).showLater();
        }
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
                protected void run(Result result) throws Throwable {
                    invoke(project, null, file);
                }
            }.execute();
        }
    }
}
