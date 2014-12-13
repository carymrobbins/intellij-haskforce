package com.haskforce.utils;

import com.haskforce.index.HaskellModuleIndex;
import com.haskforce.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * General util class. Provides methods for finding named nodes in the Psi tree.
 */
public class HaskellUtil {
    /**
     * Finds name definition across all Haskell files in the project. All
     * definitions are found when name is null.
     */
    @NotNull
    public static List<PsiNamedElement> findDefinitionNode(@NotNull Project project, @Nullable String name, @Nullable PsiNamedElement e) {
        // Guess where the name could be defined by lookup up potential modules.
        final Set<String> potentialModules =
                e == null ? Collections.EMPTY_SET
                          : getPotentialDefinitionModuleNames(e, HaskellPsiUtil.parseImports(e.getContainingFile()));
        List<PsiNamedElement> result = ContainerUtil.newArrayList();
        final String qPrefix = e == null ? null : getQualifiedPrefix(e);
        final PsiFile psiFile = e == null ? null : e.getContainingFile().getOriginalFile();
        if (psiFile instanceof HaskellFile) {
            findDefinitionNode((HaskellFile)psiFile, name, e, result);
        }
        for (String potentialModule : potentialModules) {
            List<HaskellFile> files = HaskellModuleIndex.getFilesByModuleName(project, potentialModule, GlobalSearchScope.allScope(project));
            for (HaskellFile f : files) {
                final boolean returnAllReferences = name == null;
                final boolean inLocalModule = f != null && qPrefix == null && f.equals(psiFile);
                final boolean inImportedModule = f != null && potentialModules.contains(f.getModuleName());
                if (returnAllReferences || inLocalModule || inImportedModule) {
                    findDefinitionNode(f, name, e, result);
                }
            }
        }
        return result;
    }

    /**
     * Finds a name definition inside a Haskell file. All definitions are found when name
     * is null.
     */
    public static void findDefinitionNode(@Nullable HaskellFile file, @Nullable String name, @Nullable PsiNamedElement e, @NotNull List<PsiNamedElement> result) {
        if (file == null) return;
        // We only want to look for classes that match the element we are resolving (e.g. varid, conid, etc.)
        final Class<? extends PsiNamedElement> elementClass;
        if (e instanceof HaskellVarid) {
            elementClass = HaskellVarid.class;
        } else if (e instanceof HaskellConid) {
            elementClass = HaskellConid.class;
        } else {
            elementClass = PsiNamedElement.class;
        }
        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);
        for (PsiNamedElement namedElement : namedElements) {
            if ((name == null || name.equals(namedElement.getName())) && definitionNode(namedElement)) {
                result.add(namedElement);
            }
        }
    }

    /**
     * Finds a name definition inside a Haskell file. All definitions are found when name
     * is null.
     */
    @NotNull
    public static List<PsiNamedElement> findDefinitionNodes(@Nullable HaskellFile haskellFile, @Nullable String name) {
        List<PsiNamedElement> ret = ContainerUtil.newArrayList();
        findDefinitionNode(haskellFile, name, null, ret);
        return ret;
    }

    /**
     * Finds name definition across all Haskell files in the project. All
     * definitions are found when name is null.
     */
    @NotNull
    public static List<PsiNamedElement> findDefinitionNodes(@NotNull Project project) {
        return findDefinitionNode(project, null, null);
    }

    /**
     * Finds name definitions that are within the scope of a file, including imports (to some degree).
     */
    @NotNull
    public static List<PsiNamedElement> findDefinitionNodes(@NotNull HaskellFile psiFile) {
        List<PsiNamedElement> result = findDefinitionNodes(psiFile, null);
        result.addAll(findDefinitionNode(psiFile.getProject(), null, null));
        return result;
    }

    /**
     * Tells whether a named node is a definition node based on its context.
     *
     * Precondition: Element is in a Haskell file.
     */
    public static boolean definitionNode(@NotNull PsiNamedElement e) {
        if (e instanceof HaskellVarid) return definitionNode((HaskellVarid)e);
        if (e instanceof HaskellConid) return definitionNode((HaskellConid)e);
        return false;
    }

    public static boolean definitionNode(@NotNull HaskellConid e) {
        final HaskellConstr constr = PsiTreeUtil.getParentOfType(e, HaskellConstr.class);
        final HaskellCon con;
        if (constr != null) {
            con = constr.getCon();
        } else {
            final HaskellNewconstr newconstr = PsiTreeUtil.getParentOfType(e, HaskellNewconstr.class);
            con = newconstr == null ? null : newconstr.getCon();
        }
        final HaskellConid conid = con == null ? null : con.getConid();
        return e.equals(conid);
    }

    public static boolean definitionNode(@NotNull HaskellVarid e) {
        final PsiElement parent = e.getParent();
        if (parent == null) return false;
        // If we are in a variable declaration (which has a type signature), return true.
        if (HaskellPsiUtil.isType(parent, HaskellTypes.VARS)) return true;
        // If the parent is not a top-level declaration (Funorpatdecl), then it must be a varop (infix declaration)
        // and the next parent up must be a top-level declaration.
        if (!(parent instanceof HaskellFunorpatdecl)) {
            if (!(parent instanceof HaskellVarop && parent.getParent() instanceof HaskellFunorpatdecl)) {
                return false;
            }
        }
        // Now we have to figure out if the current varid, e, is the first top-level declaration in the file.
        // Check each top-level declaration.  When we find the first one that matches our element's name we'll return
        // true if the elements are equal, false otherwise.
        final String name = e.getName();
        for (HaskellFunorpatdecl f : PsiTreeUtil.findChildrenOfType(e.getContainingFile(), HaskellFunorpatdecl.class)) {
            final HaskellVarop varop = f.getVarop();
            // Check if the function is defined as infix.
            if (varop != null) {
                final HaskellVarid varid = varop.getVarid();
                if (varid != null && name.equals(varid.getName())) {
                    return e.equals(varid);
                }
            } else {
                // There can be multiple varids in a declaration, so we'll need to grab the first one.
                List<HaskellVarid> varids = f.getVaridList();
                if (varids.size() > 0) {
                    final HaskellVarid varid = varids.get(0);
                    if (name.equals(varid.getName())) {
                        return e.equals(varid);
                    }
                }
            }
        }
        return false;
    }

    /**
     * Tells whether a node is a definition node based on its context.
     */
    public static boolean definitionNode(@NotNull ASTNode node) {
        final PsiElement element = node.getPsi();
        return element instanceof PsiNamedElement && definitionNode((PsiNamedElement)element);
    }

    @Nullable
    public static String getQualifiedPrefix(@NotNull PsiElement e) {
        final PsiElement q = PsiTreeUtil.getParentOfType(e, HaskellQcon.class, HaskellQvar.class);
        if (q == null) { return null; }
        final String qText = q.getText();
        final int lastDotPos = qText.lastIndexOf('.');
        if (lastDotPos == -1) { return null; }
        return qText.substring(0, lastDotPos);
    }

    @NotNull
    public static Set<String> getPotentialDefinitionModuleNames(@NotNull PsiElement e, @NotNull List<HaskellPsiUtil.Import> imports) {
        final String qPrefix = getQualifiedPrefix(e);
        if (qPrefix == null) { return HaskellPsiUtil.getImportModuleNames(imports); }
        Set<String> result = new HashSet<String>(2);
        for (HaskellPsiUtil.Import anImport : imports) {
            if (qPrefix.equals(anImport.module) || qPrefix.equals(anImport.alias)) {
                result.add(anImport.module);
            }
        }
        return result;
    }
}
