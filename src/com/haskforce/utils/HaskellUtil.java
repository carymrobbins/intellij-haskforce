package com.haskforce.utils;

import com.haskforce.HaskellFileType;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellTypes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.indexing.FileBasedIndex;

import java.util.Collection;
import java.util.List;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class HaskellUtil {
    /**
     * Finds name across all project Haskell files.
     */
    public static List<PsiNamedElement> findDefinitionNode(Project project, String name) {
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
                    if (name.equals(property.getName()) && definitionNode(property)) {
                        if (result == null) {
                            result = ContainerUtil.newArrayList();
                        }
                        result.add(property);
                    }
                }
            }
        }
        return result != null ? result : ContainerUtil.<PsiNamedElement>emptyList();
    }

    public static List<PsiNamedElement> findNamedNodes(Project project) {
        List<PsiNamedElement> result = ContainerUtil.newArrayList();
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

    /**
     * Tells whether a named node is a definition node based on its context.
     *
     * Precondition: Element is in a Haskell file.
     */
    public static boolean definitionNode(PsiNamedElement e) {
        // Type signatures "pattern".
        if (e.getParent().getNode().getElementType().equals(HaskellTypes.VARS)) {
            return true;
        }
        return false;
    }
}
