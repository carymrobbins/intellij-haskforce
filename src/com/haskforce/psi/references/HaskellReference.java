package com.haskforce.psi.references;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.index.HaskellModuleIndex;
import com.haskforce.psi.*;
import com.haskforce.psi.impl.HaskellPsiImplUtil;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Resolves references to elements. Will be used in Go To Symbol as well as its inverse, Find Usages.
 * Currently, the implementation of the multiresolve is very 'patchy', as it took quite a few tries to get
 * most (hopefully all) cases covered. The class has been developed totally test first, so refactoring  to a better
 * structured implementation should be feasible.
 * It draws heavily on functions that are implemented in the HaskellUtil class. Those functions are not
 * tested in the HaskellUtil class but are also tested through the HaskellGoToSymbolTest class (as is this class)
 *
 * There are also some tests present in the HaskellRenameTest. Not all test cases could be fitted into that approach.
 * My hypothesis is that the test framework doesn't like the 'ref' and 'resolve' tags in the same file,  as it always
 * only seemed to want to find one of both.
 */
public class HaskellReference extends PsiReferenceBase<PsiNamedElement> implements PsiPolyVariantReference {
    private String name;

    public HaskellReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        ((PsiNamedElement)this.myElement).replace(element);
        return element;
    }

    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    /**
     * Resolves references to a set of results.
     */
    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        if(myElement instanceof HaskellQconid){
            PsiElement parent = ((HaskellQconid) myElement).getParent();
            if( parent instanceof HaskellImpdecl){
                HaskellQconid haskellQconid = (HaskellQconid) myElement;
                String moduleName = haskellQconid.getText();
                List<HaskellFile> filesByModuleName = HaskellModuleIndex.getFilesByModuleName(myElement.getProject(), moduleName, GlobalSearchScope.projectScope(myElement.getProject()));
                List<ResolveResult> results = new ArrayList<ResolveResult>(20);
                for (HaskellFile haskellFile : filesByModuleName) {
                    Collection<HaskellModuledecl> childrenOfType = PsiTreeUtil.findChildrenOfType(haskellFile, HaskellModuledecl.class);
                    HaskellModuledecl next = childrenOfType.iterator().next();
                    HaskellQconid qconid = next.getQconid();
                    if (qconid != null) {
                        results.add(new PsiElementResolveResult(qconid));
                    }
                }
                return results.toArray(new ResolveResult[results.size()]);
            }
            if (parent instanceof  HaskellModuledecl){
//                return new ResolveResult[] {new PsiElementResolveResult(myElement)};
                return EMPTY_RESOLVE_RESULT;
            }
        }
        // We should only be resolving varids or conids.
        if (!(myElement instanceof HaskellVarid || myElement instanceof HaskellConid)) {
            return EMPTY_RESOLVE_RESULT;
        }

        Project project = myElement.getProject();
        final List<PsiNamedElement> namedElements = HaskellUtil.findDefinitionNode(project, name, myElement);
        // Make sure that we only complete the last conid in a qualified expression.
        if (myElement instanceof HaskellConid) {
            // Don't resolve a module import to a constructor.
            HaskellQconid qconid = PsiTreeUtil.getParentOfType(myElement, HaskellQconid.class);
            if (qconid == null) {
                /**
                 * Are we in a qualified variable call?
                 */
                HaskellQvarid haskellQvarid = PsiTreeUtil.getParentOfType(myElement, HaskellQvarid.class);
                if(haskellQvarid != null){
                    String fullQualifierAndFunctionName = haskellQvarid.getText();
                    int i1 = StringUtils.lastIndexOf(fullQualifierAndFunctionName, '.');
                    String fullQualifierName = StringUtils.substring(fullQualifierAndFunctionName, 0, i1);
                    String functionName = StringUtils.substring(fullQualifierAndFunctionName,i1+1);

                    List<HaskellConid> conidList = haskellQvarid.getConidList();
                    int i = 0;
                    for (; i < conidList.size(); i++) {
                        if (conidList.get(i).equals(myElement)){
                            break;
                        }
                    }

                    /**
                     * TODO still take care of the situation where you import
                     * A.B as AB as well as A.C as AB. Only point to one of both
                     */

                    HaskellFile containingFile = (HaskellFile)myElement.getContainingFile();
                    List<HaskellImpdecl> importDeclarations = containingFile.getBody().getImpdeclList();
                    for (HaskellImpdecl importDeclaration : importDeclarations) {
                        List<HaskellQconid> qconidList = importDeclaration.getQconidList();
                        if (importDeclaration.getQualified() != null){
                            HaskellQconid moduleName = Iterables.getFirst(qconidList, null);
                            HaskellQconid last = Iterables.getLast(qconidList);
                            if (fullQualifierName.equals(last.getText())){
                                List<HaskellFile> filesByModuleName = HaskellModuleIndex.getFilesByModuleName(myElement.getProject(), moduleName.getText(), GlobalSearchScope.projectScope(myElement.getProject()));
                                List<PsiNamedElement> definitionNodes = HaskellUtil.findDefinitionNodes(filesByModuleName.get(0), functionName);
                                for (PsiNamedElement definitionNode : definitionNodes) {
                                    if (definitionNode.getName().equals(functionName)){
                                        HaskellConid haskellConid = last.getConidList().get(i);
                                        return new ResolveResult[]{new PsiElementResolveResult(haskellConid)};
                                    }
                                }
                            }
                        }

                    }

                } else {
                    if (myElement.getParent() instanceof HaskellTycon){
                        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
                        for (PsiNamedElement namedElement : namedElements){
                            if (namedElement.getParent() instanceof HaskellTycon){
                                results.add(new PsiElementResolveResult(namedElement));
                            }
                        }
                        return results.toArray(new ResolveResult[results.size()]);
                    }
                    return EMPTY_RESOLVE_RESULT;
                }
            }

        }

        /**
         * Do not resolve module to constructor. In fact, do not resolve module as it's a declaration itself.
         */
        HaskellModuledecl moduleDecl = PsiTreeUtil.getParentOfType(myElement, HaskellModuledecl.class);
        if(moduleDecl != null){
            /**
             * are you the module name?
             */
            List<HaskellConid> conidList = moduleDecl.getQconid().getConidList();
            if (myElement.equals(conidList.get(conidList.size() -1))) {
                return EMPTY_RESOLVE_RESULT;
            } else {
                /*
                Resolve all cons that aren't the module name to the folder they belong to. Because that's the only
                single instance where all those cons point to (A.B.C, A and B point to folders A and B)
                 */
                PsiDirectory topDirectory = myElement.getContainingFile().getContainingDirectory();
                List<HaskellConid> reversedConIdList = Lists.reverse(conidList);
                //skip module name which is after reversal in front of the list
                Iterator<HaskellConid> reversedConidIterator = reversedConIdList.iterator();
                reversedConidIterator.next();
                while(reversedConidIterator.hasNext()){
                    HaskellConid haskellConid = reversedConidIterator.next();
                    if (myElement.equals(haskellConid)){
                        List<PsiElementResolveResult> results = Lists.newArrayList();
                        results.add(new PsiElementResolveResult(topDirectory));
                        return results.toArray(new ResolveResult[results.size()]);
                    }
                    topDirectory = topDirectory.getParentDirectory();
                }

            }

        }


        HaskellImpdecl haskellImpdecl = PsiTreeUtil.getParentOfType(myElement, HaskellImpdecl.class);
        if (haskellImpdecl != null){
            PsiElement parent = myElement.getParent();
            if (parent instanceof  HaskellQconid) {
                HaskellQconid haskellQconid = (HaskellQconid) parent;
                /**
                 * Taking the first qcon here. There are at maximum two qcons, the first one
                 * containing the list of cons that denote the folders of the import statement.
                 * The second one is the one used in case of a qualified import.
                 * So, in import qualified A.B.C.ModuleName as A.B.C.ModuleName2, qcon[0]
                 * will be A.B.C.ModuleName, qcon[1] will be A.B.C.ModuleName2
                 */
                if (haskellImpdecl.getQconidList().get(0).equals(haskellQconid)) {
                    List<HaskellConid> conidList = haskellQconid.getConidList();
                    for (int i = 0; i < conidList.size(); i++) {
                        if(myElement.equals(conidList.get(i))){
                            List<PsiElementResolveResult> results = handleImportReferences(haskellImpdecl, Iterables.getLast(conidList), i);
                            return results.toArray(new ResolveResult[results.size()]);
                        }
                    }
                }

                return EMPTY_RESOLVE_RESULT;
            }
        }



        /**
         * TODO
         * Would like to use this StubIndex, but using it here creates problems when performing go to symbol,
         * ends up in a neverending recursive call and a nice stackoverflow error. Don't know why.
         *
         * https://devnet.jetbrains.com/thread/459789
         */
//        GlobalSearchScope scope = GlobalSearchScope.allScope(project);
//        Collection<HaskellNamedElement> namedElements = StubIndex.getElements(HaskellAllNameIndex.KEY, name, project, scope, HaskellNamedElement.class);

        // Guess 20 variants tops most of the time in any real code base.

        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
        List<HaskellPsiUtil.Import> imports = HaskellPsiUtil.parseImports(myElement.getContainingFile());

        String qualifiedCallName = HaskellUtil.getQualifiedPrefix(myElement);

        if (qualifiedCallName == null){
            /**
             * This is a set because sometimes there seems to be overlap between findDefintionNode which
             * should return all 'left-most' variables and the local variables. Also called
             *  a stop gap.
             */
            Set<PsiElement> resultSet = Sets.newHashSet();
            resultSet.addAll(HaskellUtil.matchGlobalNamesUnqualified(myElement,namedElements,imports));

            List<PsiElement> localVariables = HaskellUtil.matchLocalDefinitionsInScope(myElement, name);
            for (PsiElement psiElement : localVariables) {
                resultSet.add(psiElement);
            }

            /**
             * TODO find out if we can or can not check the where clauses in case we found something higher up (a
             * let clause or so). Not yet sure about the correct precedence.
             */
            List<PsiElement> localWhereDefinitions = HaskellUtil.matchWhereClausesInScope(myElement, name);
            for (PsiElement element : localWhereDefinitions) {
                resultSet.add(element);
            }
            Iterator<PsiElement> iterator = resultSet.iterator();
            while(iterator.hasNext()){
                results.add(new PsiElementResolveResult(iterator.next()));
            }
            return results.toArray(new ResolveResult[results.size()]);

        } else {
            results.addAll(HaskellUtil.matchGlobalNamesQualified(namedElements, qualifiedCallName, imports));
            return results.toArray(new ResolveResult[results.size()]);
        }
    }

    private @NotNull List<PsiElementResolveResult> handleImportReferences(@NotNull HaskellImpdecl haskellImpdecl,
                                                                          @NotNull PsiNamedElement myElement, int i) {
        /**
         * Don't use the named element yet to determine which element we're
         * talking about, not necessary yet
         */
        List<PsiElementResolveResult> results = new ArrayList<PsiElementResolveResult>();
        List<HaskellQconid> qconidList = haskellImpdecl.getQconidList();

        if (qconidList.size() > 0){
            List<HaskellConid> conids = qconidList.get(0).getConidList();
            if (i == conids.size() - 1) {
                String moduleName = qconidList.get(0).getText();

                GlobalSearchScope globalSearchScope = GlobalSearchScope.projectScope(myElement.getProject());
                List<HaskellFile> filesByModuleName = HaskellModuleIndex.getFilesByModuleName(myElement.getProject(), moduleName, globalSearchScope);
                for (HaskellFile haskellFile : filesByModuleName) {
                    HaskellModuledecl[] moduleDecls = PsiTreeUtil.getChildrenOfType(haskellFile, HaskellModuledecl.class);
                    if (moduleDecls.length != 0) {
                        List<HaskellConid> conidList = moduleDecls[0].getQconid().getConidList();
                        results.add(new PsiElementResolveResult(conidList.get(i)));
                    }
                }
            } else {
                //Not dealing with the module name part of an import declaration. Try to point to the correct psidirectory
                // 'i' denotes the index of the folder in the import statement. So, in A.B.C.ModuleName, i = 2 points to C.
                // containingDirectory will be A.B.C in this case. We need to determine how many times we need to perform
                // 'getParentDirectory'. This is (qconidList.size() -1) - i; Because we take the first directory already
                //before the loop starts, we need to do -2. This could get simpler I think.
                PsiDirectory containingDirectory = myElement.getContainingFile().getParent();
                for (int j =0; j < conids.size()- 2 - i;j++){
                    containingDirectory = containingDirectory.getParentDirectory();
                }
                results.add(new PsiElementResolveResult(containingDirectory));
            }
        }
        return results;


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
