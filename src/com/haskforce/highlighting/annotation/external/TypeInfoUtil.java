package com.haskforce.highlighting.annotation.external;

import com.haskforce.settings.ToolKey;
import com.haskforce.utils.ExecUtil;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiUtilBase;


public class TypeInfoUtil {
    public static String getTypeInfo(Project project) {
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(
                project);
        if (fileEditorManager == null){
            return null;
        }
        /*
        This call always generates a rather large stacktrace, but succeeds nevertheless.
        Best find out why.
         */
        Editor textEditor = fileEditorManager.getSelectedTextEditor();
        if (textEditor == null){
            return null;
        }

        VisualPosition blockStart = correctFor0BasedVS1Based(textEditor.getSelectionModel().getSelectionStartPosition());
        VisualPosition blockEnd = correctFor0BasedVS1Based(textEditor.getSelectionModel().getSelectionEndPosition());
        if (blockStart == null || blockEnd == null){
            return null;
        }

        PsiFile psiFileInEditor = PsiUtilBase.getPsiFileInEditor(textEditor, project);
        if(psiFileInEditor == null){
            return "no psi file found";
        }
        VirtualFile projectFile = psiFileInEditor.getVirtualFile();
        if (projectFile == null){
            return "project file is null";
        }
        final String canonicalPath = projectFile.getCanonicalPath();
        if (canonicalPath == null){
            return "canonical path is null";
        }

        final String workDir = ExecUtil.guessWorkDir(project, projectFile);
        if (ToolKey.GHC_MODI_KEY.getPath(project) != null) {
            final Module module = ModuleUtilCore.findModuleForFile(projectFile, project);
            if (module != null) {
                GhcModi ghcModi = module.getComponent(GhcModi.class);
                if (ghcModi != null) {
                    return GhcModi.getFutureType(project, ghcModi.type(canonicalPath,
                            blockStart,blockEnd));

                } else {
                    return "ghcModi == null";
                }
            } else {
                return "module == null";
            }
        } else {
            return GhcMod.type(project, workDir, canonicalPath, blockStart, blockEnd);
        }
    }

    private static VisualPosition correctFor0BasedVS1Based(VisualPosition pos0Based) {
        if (pos0Based == null){
            return null;
        }
        return new VisualPosition(pos0Based.line+1,pos0Based.column+1);
    }
}
