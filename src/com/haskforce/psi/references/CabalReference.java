package com.haskforce.psi.references;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.haskforce.cabal.psi.CabalModule;
import com.haskforce.cabal.psi.CabalVarid;
import com.haskforce.cabal.psi.impl.CabalPsiImplUtil;
import com.haskforce.index.HaskellModuleIndex;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellModuledecl;
import com.haskforce.psi.HaskellQconid;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class CabalReference extends PsiReferenceBase<PsiNamedElement> implements PsiPolyVariantReference {
    private String name;

    public CabalReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    @Override
    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        /**
         * Not sure this is totally canonical. I didn't find an indicator how to implement this method correctly,
         * not even in the java or groovy plugins. Couldn't find my way to the correct reference maybe, but all
         * implementations I saw up until now did not perform this replace, just returned the element that it passed in.
         * They implemented an 'id' function so to speak.
         * I'm a bit afraid that this is going to trigger a rename for every element.
         */
        this.myElement.replace(element);
        return element;
    }

    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean b) {
        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
        if (myElement instanceof CabalVarid){
            PsiElement parent = myElement.getParent();
            if(parent instanceof  CabalModule){
                CabalModule cabalModule =(CabalModule) parent;
                Project project = myElement.getProject();
                String moduleName = cabalModule.getText();
                List<HaskellFile> allHaskellFiles = HaskellModuleIndex.getFilesByModuleName(project, moduleName,
                        GlobalSearchScope.allScope(project));
                /**
                 * Finding two modules with the same name and package is a bit weird.
                 **/
                assert (allHaskellFiles.size() <= 1);

                /**
                 * Very similar to code in HaskellReference. Need to find a way to remove pieces of duplication.
                 */
                for (HaskellFile haskellFile : allHaskellFiles) {
                    List<CabalVarid> varidList = cabalModule.getVaridList();
                    if (myElement.equals (Iterables.getLast(varidList))){
                        /**
                         * It's the module name
                         */
                        HaskellModuledecl haskellModuledecl = PsiTreeUtil.findChildOfType(haskellFile, HaskellModuledecl.class);
                        HaskellConid haskellModuleName = Iterables.getLast(haskellModuledecl.getQconid().getConidList());
                        results.add(new PsiElementResolveResult(haskellModuleName));
                    } else {
                        List<CabalVarid> revertedVaridList = Lists.reverse(varidList);
                        Iterator<CabalVarid> iterator = revertedVaridList.iterator();
                        /**
                         * Get rid of the module name
                         */
                        iterator.next();
                        PsiDirectory containingDirectory = haskellFile.getContainingDirectory();
                        while(iterator.hasNext()){
                            /**
                             * Verify whether this approach works with stuff like A.A.Something. Should be because
                             * comparing on varid should not just compare on text representation of the varid.
                             */
                            CabalVarid next = iterator.next();
                            if (myElement.equals(next)){
                                results.add(new PsiElementResolveResult(containingDirectory));
                                break;
                            }
                            containingDirectory = containingDirectory.getParent();
                        }
                    }
                }
            }
        }

        if (myElement instanceof CabalModule){
            CabalModule cabalModule =(CabalModule)myElement;
            String moduleName = cabalModule.getText();
            Project project = myElement.getProject();
            List<HaskellFile> allHaskellFiles = HaskellModuleIndex.getFilesByModuleName(project, moduleName,
                    GlobalSearchScope.allScope(project));
            for (HaskellFile haskellFile : allHaskellFiles) {
                HaskellModuledecl haskellModule = PsiTreeUtil.getChildOfType(haskellFile, HaskellModuledecl.class);
                if (haskellModule == null){
                    continue;
                }
                HaskellQconid qconid = haskellModule.getQconid();
                if (qconid != null) {
                    results.add(new PsiElementResolveResult(qconid));
                }
            }
        }

        return results.toArray(new ResolveResult[results.size()]);
    }

    /**
     * Resolves references to a single result, or fails.
     */
    @Nullable
    @Override
    public PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(false);
        return resolveResults.length == 1 ? resolveResults[0].getElement() : null;
    }


    @NotNull
    @Override
    public Object[] getVariants() {
        /**
         * No auto complete for now.
         */
        return new Object[0];
    }

    @Override
    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        PsiElement element;
        if (myElement instanceof CabalVarid && myElement.getParent() instanceof CabalModule){
            element = CabalPsiImplUtil.setName((CabalVarid)myElement,newElementName);
            if (element != null) return element;
            throw new IncorrectOperationException("cannot rename "+name + "to "+newElementName);
        }
        return super.handleElementRename(newElementName);
    }
}
