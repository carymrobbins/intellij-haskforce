package com.haskforce.psi.references;

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
        PsiElement cabalModule = myElement.getParent();
        if ( myElement instanceof CabalVarid && cabalModule instanceof CabalModule) {
            String moduleName = cabalModule.getText();
            Project project = myElement.getProject();
            List<HaskellFile> allHaskellFiles = HaskellModuleIndex.getFilesByModuleName(project, moduleName,
                    GlobalSearchScope.allScope(project));
            if (allHaskellFiles.size() == 0){
                return new ResolveResult[0];
            }
            List<CabalVarid> varidList = ((CabalModule) cabalModule).getVaridList();
            int moduleNumber = 0;
            for (CabalVarid cabalVarid : varidList) {
                if (cabalVarid .equals(myElement)){
                    break;
                }
                ++moduleNumber;
            }
            for (HaskellFile haskellFile : allHaskellFiles) {
                HaskellModuledecl haskellModule = PsiTreeUtil.getChildOfType(haskellFile, HaskellModuledecl.class);
                if(haskellModule == null){
                    continue;
                }
                HaskellQconid qconid = haskellModule.getQconid();
                if(qconid == null){
                    continue;
                }

                List<HaskellConid> conidList = qconid.getConidList();
                HaskellConid haskellConid = conidList.get(moduleNumber);
                results.add(new PsiElementResolveResult(haskellConid));

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
