package com.haskforce;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.indexing.FileBasedIndex;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellProperty;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class HaskellUtil {
    public static List<HaskellProperty> findProperties(Project project, String key) {
        List<HaskellProperty> result = null;
        Collection<VirtualFile> virtualFiles = FileBasedIndex.getInstance().getContainingFiles(FileTypeIndex.NAME, HaskellFileType.INSTANCE,
                GlobalSearchScope.allScope(project));
        for (VirtualFile virtualFile : virtualFiles) {
            HaskellFile haskellFile = (HaskellFile) PsiManager.getInstance(project).findFile(virtualFile);
            if (haskellFile != null) {
                HaskellProperty[] properties = PsiTreeUtil.getChildrenOfType(haskellFile, HaskellProperty.class);
                if (properties != null) {
                    for (HaskellProperty property : properties) {
                        if (key.equals(property.getKey())) {
                            if (result == null) {
                                result = new ArrayList<HaskellProperty>();
                            }
                            result.add(property);
                        }
                    }
                }
            }
        }
        return result != null ? result : Collections.<HaskellProperty>emptyList();
    }

    public static List<HaskellProperty> findProperties(Project project) {
        List<HaskellProperty> result = new ArrayList<HaskellProperty>();
        Collection<VirtualFile> virtualFiles = FileBasedIndex.getInstance().getContainingFiles(FileTypeIndex.NAME, HaskellFileType.INSTANCE,
                GlobalSearchScope.allScope(project));
        for (VirtualFile virtualFile : virtualFiles) {
            HaskellFile haskellFile = (HaskellFile) PsiManager.getInstance(project).findFile(virtualFile);
            if (haskellFile != null) {
                HaskellProperty[] properties = PsiTreeUtil.getChildrenOfType(haskellFile, HaskellProperty.class);
                if (properties != null) {
                    Collections.addAll(result, properties);
                }
            }
        }
        return result;
    }
}
