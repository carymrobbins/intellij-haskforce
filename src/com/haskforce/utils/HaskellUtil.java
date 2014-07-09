package com.haskforce.utils;

import com.haskforce.HaskellFileType;
import com.haskforce.psi.HaskellFile;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.indexing.FileBasedIndex;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class HaskellUtil {
    public static List<PsiNamedElement> findNamedNode(Project project, String name) {
        List<PsiNamedElement> result = null;
        Collection<VirtualFile> virtualFiles =
                FileBasedIndex.getInstance().getContainingFiles(FileTypeIndex.NAME,
                        HaskellFileType.INSTANCE, GlobalSearchScope.allScope(project));
        for (VirtualFile virtualFile : virtualFiles) {
            HaskellFile haskellFile = (HaskellFile) PsiManager.getInstance(project).findFile(virtualFile);
            if (haskellFile != null) {
                Collection<PsiNamedElement> namedElements =
                        PsiTreeUtil.findChildrenOfAnyType(haskellFile, PsiNamedElement.class);
                for (PsiNamedElement property : namedElements) {
                    if (name.equals(property.getName())) {
                        if (result == null) {
                            result = new ArrayList<PsiNamedElement>(200);
                        }
                        result.add(property);
                    }
                }
            }
        }
        return result != null ? result : Collections.<PsiNamedElement>emptyList();
    }

    public static List<PsiNamedElement> findNamedNodes(Project project) {
        List<PsiNamedElement> result = new ArrayList<PsiNamedElement>(200);
        Collection<VirtualFile> virtualFiles = FileBasedIndex.getInstance().getContainingFiles(FileTypeIndex.NAME, HaskellFileType.INSTANCE,
                GlobalSearchScope.allScope(project));
        for (VirtualFile virtualFile : virtualFiles) {
            HaskellFile simpleFile = (HaskellFile) PsiManager.getInstance(project).findFile(virtualFile);
            if (simpleFile != null) {
                Collection<PsiNamedElement> namedElements =
                        PsiTreeUtil.findChildrenOfAnyType(simpleFile, PsiNamedElement.class);
                result.addAll(namedElements);
            }
        }
        return result;
    }
}
