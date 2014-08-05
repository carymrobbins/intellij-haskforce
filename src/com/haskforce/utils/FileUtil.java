package com.haskforce.utils;

import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

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

    /**
     * Returns an array of directory names as the relative path to `file` from the source root.
     * For example, given file "project/src/foo/bar/baz.hs" the result would be `{"foo", "bar"}`.
     */
    public static @Nullable List<String> getPathFromSourceRoot(Project project, VirtualFile file) {
        VirtualFile root = getSourceRoot(project, file);
        final String rootPath;
        if (root == null || (rootPath = root.getCanonicalPath()) == null) {
            return null;
        }
        ArrayList<String> result = new ArrayList<String>(0);
        VirtualFile directory = file;
        while (directory != null) {
            if (rootPath.equals(directory.getCanonicalPath())) {
                return result;
            }
            result.add(0, directory.getName());
            directory = directory.getParent();
        }
        return null;
    }

    public static @Nullable VirtualFile getSourceRoot(Project project, VirtualFile file) {
        if (project == null || file == null) {
            return null;
        }
        VirtualFile[] roots = ProjectRootManager.getInstance(project).getContentSourceRoots();
        for (VirtualFile root : roots) {
            final String rootPath = root.getCanonicalPath();
            if (rootPath == null) {
                continue;
            }
            VirtualFile directory = file;
            while (directory != null) {
                if (rootPath.equals(directory.getCanonicalPath())) {
                    return directory;
                }
                directory = directory.getParent();
            }
        }
        return null;
    }
}
