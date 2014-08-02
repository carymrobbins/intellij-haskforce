package com.haskforce.utils;

import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;

public class FileUtil {
    public static void updateFileText(final Project project, final PsiFile file, final Function<String, String> function) {
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
                        app.runWriteAction(new Runnable() {
                            @Override
                            public void run() {
                                document.setText(function.fun(document.getText()));
                            }
                        });
                    }
                }, "Update text for " + file.getName(), "", document);
            }
        });
    }
}
