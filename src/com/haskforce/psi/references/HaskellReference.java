package com.haskforce.psi.references;

import com.google.common.collect.Iterables;
import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.psi.*;
import com.haskforce.psi.impl.HaskellPsiImplUtil;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
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
         * TODO
         * Would like to use this StubIndex, but using it here creates problems when performing go to symbol,
         * ends up in a neverending recursive call and a nice stackoverflow error. Don't know why.
         */
//        GlobalSearchScope scope = GlobalSearchScope.allScope(project);
//        Collection<HaskellNamedElement> namedElements = StubIndex.getElements(HaskellAllNameIndex.KEY, name, project, scope, HaskellNamedElement.class);

        // Guess 20 variants tops most of the time in any real code base.
        final List<PsiNamedElement> namedElements = HaskellUtil.findDefinitionNode(project, name, myElement);
        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
        String ownModuleName = getModuleName(myElement);
        List<HaskellImpdecl> importDeclarations = getImportDeclarations(myElement);

        String qualifiedCallName = extractQualifiedCallName();

        for (PsiNamedElement property : namedElements) {
           String moduleName = getModuleName(property);
           if (qualifiedCallName.isEmpty() &&
                   (importPresent (moduleName, importDeclarations) || ownModuleName.equals(moduleName))) {
            //noinspection ObjectAllocationInLoop
               results.add(new PsiElementResolveResult(property));
            }
            if (! qualifiedCallName.isEmpty() ){
                HaskellImpdecl correspondingImportDeclaration = findCorrespondingImportDeclaration(qualifiedCallName, importDeclarations);
                if(correspondingImportDeclaration != null &&
                    namedElementDefinedInCorrespondingModule(correspondingImportDeclaration,moduleName)){
                  results.add(new PsiElementResolveResult(property));
                }
            }
        }
        if (qualifiedCallName.isEmpty()) {
            PsiElement localElement = checkLocalDefinitions();
            if (localElement != null) {
                results.add(new PsiElementResolveResult(localElement));
            }
            PsiElement psiElement = checkForWhereClauses(myElement);
            if (psiElement != null){
                results.add(new PsiElementResolveResult(psiElement));
            }
        }
        return results.toArray(new ResolveResult[results.size()]);
    }

    private PsiElement checkForWhereClauses(PsiNamedElement myElement) {
        PsiElement parent = myElement.getParent();
        do {
            if (parent instanceof HaskellRhs) {
                HaskellRhs rhs = (HaskellRhs) parent;
                PsiElement where = rhs.getWhere();
                if (where == null) {
                    return null;
                } else {
                    PsiElement psiElement = checkWhereClause(where);
                    if (psiElement != null) {
                        return psiElement;
                    }
                }
            }
            parent = parent.getParent();
        } while (! (parent instanceof  HaskellBody));

        return null;
    }

    private @Nullable PsiElement checkWhereClause(@NotNull PsiElement where) {
        PsiElement nextSibling = where.getNextSibling();
        while(nextSibling != null){
            if(nextSibling instanceof HaskellFunorpatdecl) {
                PsiElement psiElement = HaskellUtil.lookForFunOrPatDeclWithCorrectName(nextSibling, name);
                if (psiElement != null){
                    return psiElement;
                }
            }
            nextSibling = nextSibling.getNextSibling();
        }
        return null;
    }

    private String extractQualifiedCallName() {
        if (myElement instanceof HaskellVarid) {
            HaskellVarid haskellVarid = (HaskellVarid) myElement;
            PsiElement parent = haskellVarid.getParent();
            if (parent instanceof  HaskellQvarid){
                HaskellQvarid haskellQvarid = (HaskellQvarid) parent;
                List<HaskellConid> conidList = haskellQvarid.getConidList();
                StringBuilder qualifiedCallName = new StringBuilder();
                for (HaskellConid haskellConid : conidList) {
                    qualifiedCallName.append((haskellConid.getName()));
                    qualifiedCallName.append('.');
                }
                if (qualifiedCallName.length() >0) {
                    qualifiedCallName.deleteCharAt(qualifiedCallName.length() - 1);
                    return qualifiedCallName.toString();
                }

            }
        }

        return "";
    }

    private boolean namedElementDefinedInCorrespondingModule(HaskellImpdecl correspondingImportDeclaration, String moduleName) {
        HaskellQconid haskellQconid = correspondingImportDeclaration.getQconidList().get(0);
        StringBuilder definitionModuleNameBuilder = new StringBuilder();

        for (HaskellConid haskellConid : haskellQconid.getConidList()) {
            definitionModuleNameBuilder.append(haskellConid.getName());
            definitionModuleNameBuilder.append('.');
        }

        definitionModuleNameBuilder.deleteCharAt(definitionModuleNameBuilder.length() - 1);
        String definitionModuleName = definitionModuleNameBuilder.toString();
        return definitionModuleName.equals(moduleName);

    }

    private HaskellImpdecl findCorrespondingImportDeclaration(String qualifiedCallName, List<HaskellImpdecl> importDeclarations) {
        for (HaskellImpdecl importDeclaration : importDeclarations) {
            List<HaskellQconid> qconidList = importDeclaration.getQconidList();
            if (qconidList.size() == 2){
                /**
                 * TODO : this '2' is a guess currently. Seems to be 1 qconlist if
                 * there is only import A.B.C, two qconlist if there is
                 * import A.B.C as D.E.F
                 */
                HaskellQconid haskellQconid = qconidList.get(1);
                StringBuilder qualifiedImportBuilder = new StringBuilder();

                for (HaskellConid haskellConid : haskellQconid.getConidList()) {
                    qualifiedImportBuilder.append(haskellConid.getName());
                    qualifiedImportBuilder.append('.');
                }

                qualifiedImportBuilder.deleteCharAt(qualifiedImportBuilder.length() - 1);
                String qualifiedImportName = qualifiedImportBuilder.toString();
                if (qualifiedCallName.equals(qualifiedImportName)){
                    return importDeclaration;
                }

            }
        }
        return null;
    }

    /**
     * Not going to do it like this, going to this once and create a map of or so
     * of imported modules, going to make the look up a bit more cpu friendly.
     * For now this works.
     */
    private boolean importPresent(@NotNull String moduleName, @NotNull List<HaskellImpdecl> importDeclarations) {
        for (HaskellImpdecl importDeclaration : importDeclarations) {

            List<HaskellQconid> qconidList = importDeclaration.getQconidList();
            for (HaskellQconid haskellQconid : qconidList) {
                List<HaskellConid> conidList = haskellQconid.getConidList();
                StringBuilder moduleNameBuilder = new StringBuilder();
                for (HaskellConid haskellConid : conidList) {
                    moduleNameBuilder.append(haskellConid.getName());
                    moduleNameBuilder.append('.');
                }
                moduleNameBuilder.deleteCharAt(moduleNameBuilder.length()-1);
                if (moduleName.equals(moduleNameBuilder.toString())){
                    return true;
                }
            }
        }
        return false;
    }

    private @Nullable List<HaskellImpdecl> getImportDeclarations(@NotNull PsiNamedElement myElement) {
        PsiElement[] children = myElement.getContainingFile().getChildren();
        for (PsiElement child : children) {
            if (child instanceof HaskellBody){
                HaskellBody haskellBody = (HaskellBody) child;
                return haskellBody.getImpdeclList();
            }
        }
        return null;
    }

    private String getModuleName(@NotNull PsiNamedElement namedElement) {
        HaskellFile containingFile = (HaskellFile)namedElement.getContainingFile();
        if (containingFile == null){
            return "";
        }
        return containingFile.getModuleName();
    }

    public @Nullable PsiElement checkLocalDefinitions(){
        PsiElement parent = myElement;
        do {

            PsiElement prevSibling = parent.getPrevSibling();
            while (prevSibling != null) {
                PsiElement possibleMatch = HaskellUtil.lookForFunOrPatDeclWithCorrectName(prevSibling, name);
                if (possibleMatch != null) {
                    return possibleMatch;
                }
                if (prevSibling instanceof HaskellPat) {
                    List<HaskellVarid> varIds = HaskellUtil.extractAllHaskellVarids((HaskellPat) prevSibling);
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
            /**This is only necessary to put the caret on the declaration when Go to symbol is called
             * when the caret is on the declaration. instead of saying that it didn't find a declaration.
             * Might be that this is not that necessary (and it's once more over the tree). If it's necessary,
             * a saner implementation will be to just first check whether the caret is on a declaration and leave it there.
             */
//            PsiElement psiElement = lookForFunOrPatDeclWithCorrectName(parent);
//            if (psiElement != null) {
//                return psiElement;
//            }

        } while(! (parent instanceof  PsiFile));

        return null;
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
