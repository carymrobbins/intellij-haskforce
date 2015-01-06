package com.haskforce.psi.references;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.psi.*;
import com.haskforce.psi.impl.HaskellPsiImplUtil;
import com.haskforce.stubs.index.HaskellAllNameIndex;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Resolves references to elements.
 */
public class HaskellReference extends PsiReferenceBase<PsiNamedElement> implements PsiPolyVariantReference {
    private String name;

    public HaskellReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    /**
     * Resolves references to a set of results.
     */
    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        // We should only be resolving varids or conids.
        if (!(myElement instanceof HaskellVarid || myElement instanceof HaskellConid)) {
            return EMPTY_RESOLVE_RESULT;
        }
        // Make sure that we only complete the last conid in a qualified expression.
        if (myElement instanceof HaskellConid) {
            // Don't resolve a module import to a constructor.
            if (PsiTreeUtil.getParentOfType(myElement, HaskellImpdecl.class) != null) { return EMPTY_RESOLVE_RESULT; }
            HaskellQconid qconid = PsiTreeUtil.getParentOfType(myElement, HaskellQconid.class);
            if (qconid == null) { return EMPTY_RESOLVE_RESULT; }
            if (!myElement.equals(Iterables.getLast(qconid.getConidList()))) { return EMPTY_RESOLVE_RESULT; }
        }
        Project project = myElement.getProject();


        /**
         * This will only resolve the 'non-local' references. The local references need to be done through walking the
         * psi tree unfortunately.
         */
        GlobalSearchScope scope = GlobalSearchScope.allScope(project);
        Collection<HaskellNamedElement> namedElements = StubIndex.getElements(HaskellAllNameIndex.KEY, name, project, scope, HaskellNamedElement.class);

        // Guess 20 variants tops most of the time in any real code base.
        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
        for (PsiNamedElement property : namedElements) {
            //noinspection ObjectAllocationInLoop
            results.add(new PsiElementResolveResult(property));
        }
        PsiElement localElement = walkPsiTreeTakeTwo();
        if(localElement != null){
            results.add(new PsiElementResolveResult(localElement));
        }
        return results.toArray(new ResolveResult[results.size()]);
    }

    public PsiElement walkPsiTree(){
        PsiElement parent = myElement;
        do {
            parent = parent.getParent();
            PsiElement psiElement = lookForFunOrPatDeclWithCorrectName(parent);
            if(psiElement != null){
                return psiElement;
            }
            PsiElement[] allSiblingsOfOriginal = parent.getChildren();
            for (PsiElement sibling : allSiblingsOfOriginal) {
                PsiElement possibleMatch = lookForFunOrPatDeclWithCorrectName(sibling);
                if(possibleMatch != null){
                    return possibleMatch;
                }
            }

        } while (! (parent instanceof  PsiFile));
        return null;
    }

    public @Nullable PsiElement walkPsiTreeTakeTwo(){
        PsiElement parent = myElement;
        do {

            PsiElement prevSibling = parent.getPrevSibling();
            while (prevSibling != null) {
                PsiElement possibleMatch = lookForFunOrPatDeclWithCorrectName(prevSibling);
                if (possibleMatch != null) {
                    return possibleMatch;
                }
                if (prevSibling instanceof HaskellPat) {
                    List<HaskellVarid> varIds = Lists.newArrayList();
                    extractAllHaskellVarids(varIds, (HaskellPat) prevSibling);
                    for (HaskellVarid varId : varIds) {
                        if (name.equals(varId.getName())) {
                            return varId;
                        }
                    }
                }
                if (prevSibling instanceof HaskellVarid){
                    HaskellVarid varId = (HaskellVarid) prevSibling;
                    if (name.equals(varId.getName())){
                        return varId;
                    }
                }
                prevSibling = prevSibling.getPrevSibling();
            }
            parent = parent.getParent();
            PsiElement psiElement = lookForFunOrPatDeclWithCorrectName(parent);
            if (psiElement != null) {
                return psiElement;
            }

        } while(! (parent instanceof  PsiFile));

        return null;
    }


    private @Nullable PsiElement lookForFunOrPatDeclWithCorrectName(@NotNull PsiElement element){
        /**
         * A FunOrPatDecl with as parent haskellbody is one of the 'leftmost' function declarations.
         * Those should not be taken into account, the definition will already be found from the stub.
         * It will cause problems if we also start taking those into account over here.
         */

        if (element instanceof  HaskellFunorpatdecl &&
                ! (element.getParent() instanceof HaskellBody)) {
            PsiElement[] children = element.getChildren();
            for (PsiElement child : children) {
                if (child instanceof HaskellVarid) {
                    PsiElement psiElement = checkForMatchingVariable(child);
                    if (psiElement != null){
                        return psiElement;
                    }
                }
                if (child instanceof HaskellPat){
                    HaskellPat pat = (HaskellPat)child;
                    List<HaskellVarid> varIds = Lists.newArrayList();
                    extractAllHaskellVarids(varIds,pat);
                    for (HaskellVarid varId : varIds) {
                        if (name.equals(varId.getName())){
                            return varId;
                        }
                    }
                }
            }
        }
        return null;
    }


    private void extractAllHaskellVarids(List<HaskellVarid> varIds, HaskellPat pat) {
        /**
         * TODO
         * Best check whether this will work with the open jdk version. And Java 6.
         * Might very well not be the case.
         */
        PsiElement[] children = pat.getChildren();
        for (PsiElement child : children) {
            if (child instanceof HaskellVarid){
                varIds.add((HaskellVarid)child);
            }
            if (child instanceof HaskellPat){
                extractAllHaskellVarids(varIds, (HaskellPat)child);
            }

        }
    }

    private PsiElement checkForMatchingVariable(PsiElement child) {
        HaskellVarid haskellVarid = (HaskellVarid) child;
        if (name.equals(haskellVarid.getName())) {
            return child;
        } else {
            return null;
        }
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

    /**
     * Controls what names that get added to the autocompletion popup available
     * on ctrl-space.
     */
    @NotNull
    @Override
    public Object[] getVariants() {
        // If we are not in an expression, don't provide reference completion.
        if (PsiTreeUtil.getParentOfType(myElement, HaskellExp.class) == null) {
            return new Object[]{};
        }
        // If we are in a qualified name, don't provide reference completion.
        final PsiElement qId = PsiTreeUtil.getParentOfType(myElement, HaskellQconid.class, HaskellQvarid.class);
        if (qId != null && qId.textContains('.')) {
            return new Object[]{};
        }
        final PsiFile containingFile = myElement.getContainingFile();
        if (!(containingFile instanceof HaskellFile)) {
            return new Object[]{};
        }
        List<PsiNamedElement> namedNodes = HaskellUtil.findDefinitionNodes((HaskellFile)containingFile);
        List<LookupElement> variants = new ArrayList<LookupElement>(20);
        for (final PsiNamedElement namedElement : namedNodes) {
            final PsiElement genDecl = PsiTreeUtil.getParentOfType(namedElement, HaskellGendecl.class);
            final PsiFile psiFile = namedElement.getContainingFile();
            if (!(psiFile instanceof HaskellFile)) { continue; }
            final String module = ((HaskellFile) psiFile).getModuleOrFileName();
            final String name = namedElement.getName();
            if (name == null) { continue; }
            final String type;
            if (genDecl != null) {
                final PsiElement cType = PsiTreeUtil.getChildOfType(genDecl, HaskellCtype.class);
                type = cType == null ? "" : cType.getText();
            } else {
                type = "";
            }
            variants.add(HaskellCompletionContributor.createLookupElement(name, module, type));
        }
        return variants.toArray();
    }

    @Override
    public TextRange getRangeInElement() {
        return new TextRange(0, this.getElement().getNode().getTextLength());
    }

    /**
     * Called when renaming refactoring tries to rename the Psi tree.
     */
    @Override
    public PsiElement handleElementRename(final String newName)  throws IncorrectOperationException {
        PsiElement element;
        if (myElement instanceof HaskellVarid) {
            element = HaskellPsiImplUtil.setName((HaskellVarid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        } else if (myElement instanceof HaskellConid) {
            element = HaskellPsiImplUtil.setName((HaskellConid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        }
        return super.handleElementRename(newName);
    }
}
