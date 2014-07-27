package com.haskforce.features.intentions;

import com.haskforce.highlighting.GhcMod.*;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

public class AddTypeSignature extends BaseIntentionAction {
    public final Problem problem;

    public AddTypeSignature(Problem problem) {
        this.problem = problem;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Add type signature";
    }

    @NotNull
    @Override
    public String getText() {
        return "Add type signature";
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        // TODO: Add a setting for this.
        return true;
    }

    private static final Pattern COLON_REGEX = Pattern.compile(":");
    private static final Pattern CHAR_LIST_REGEX = Pattern.compile("\\[Char]");

    @Override
    public void invoke(@NotNull final Project project, Editor editor, final PsiFile file) throws IncorrectOperationException {
        final String[] messageParts = COLON_REGEX.split(problem.message, 2);
        if (messageParts.length != 2) {
            return;
        }
        // Substitute String for [Char] in type signatures.
        final String typeSignature = CHAR_LIST_REGEX.matcher(messageParts[1].trim()).replaceAll("String");
        final String text = file.getText();
        final int offset = StringUtil.lineColToOffset(text, problem.startLine - 1, problem.startColumn - 1);
        // TODO: Is there a better way to update the file text?
        final Application app = ApplicationManager.getApplication();
        app.saveAll();
        app.invokeLater(new Runnable() {
            @Override
            public void run() {
                final Document document = PsiDocumentManager.getInstance(project).getDocument(file);
                if (document == null) {
                    return;
                }
                CommandProcessor.getInstance().executeCommand(project, new Runnable() {
                    @Override
                    public void run() {
                        ApplicationManager.getApplication().runWriteAction(new Runnable() {
                            @Override
                            public void run() {
                                document.setText(text.substring(0, offset) + typeSignature + '\n' + text.substring(offset));
                            }
                        });
                    }
                }, "Add type signature", "", document);
            }
        });

    }
}
