package com.haskforce.haskell.psi.references;

import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.haskforce.haskell.codeInsight.LookupElementUtil;
import com.haskforce.haskell.index.HaskellModuleIndex;
import com.haskforce.haskell.psi.*;
import com.haskforce.haskell.psi.impl.HaskellPsiImplUtil;
import com.haskforce.system.utils.HaskellUtil;
import com.haskforce.system.utils.HaskellUtil.FoundDefinition;
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

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

    public static final ResolveResult[] EMPTY_RESOLVE_RESULT = new ResolveResult[0];

    /**
     * Resolves references to a set of results.
     */
    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        // We should only be resolving varids or conids.
        if (!(myElement instanceof com.haskforce.haskell.psi.HaskellVarid || myElement instanceof com.haskforce.haskell.psi.HaskellConid)) {
            return EMPTY_RESOLVE_RESULT;
        }

        Project project = myElement.getProject();
        final List<FoundDefinition> namedElements = HaskellUtil.findDefinitionNode(project, name, myElement);
        // Make sure that we only complete the last conid in a qualified expression.
        if (myElement instanceof com.haskforce.haskell.psi.HaskellConid) {
            // Don't resolve a module import to a constructor.
            com.haskforce.haskell.psi.HaskellQconid qconid = PsiTreeUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellQconid.class);
            if (qconid == null) {
                /**
                 * Are we in a qualified variable call?
                 */
                com.haskforce.haskell.psi.HaskellQvarid haskellQvarid = PsiTreeUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellQvarid.class);
                if(haskellQvarid != null){
                    String fullQualifierAndFunctionName = haskellQvarid.getText();
                    int i1 = StringUtils.lastIndexOf(fullQualifierAndFunctionName, '.');
                    String fullQualifierName = StringUtils.substring(fullQualifierAndFunctionName, 0, i1);
                    String functionName = StringUtils.substring(fullQualifierAndFunctionName,i1+1);

                    List<com.haskforce.haskell.psi.HaskellConid> conidList = haskellQvarid.getConidList();
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
                    final com.haskforce.haskell.psi.HaskellBody body = containingFile.getBody();
                    if (body == null) return EMPTY_RESOLVE_RESULT;
                    List<com.haskforce.haskell.psi.HaskellImpdecl> importDeclarations = body.getImpdeclList();
                    for (com.haskforce.haskell.psi.HaskellImpdecl importDeclaration : importDeclarations) {
                        List<com.haskforce.haskell.psi.HaskellQconid> qconidList = importDeclaration.getQconidList();
                        if (importDeclaration.getQualified() != null){
                            com.haskforce.haskell.psi.HaskellQconid moduleName = Iterables.getFirst(qconidList, null);
                            if (moduleName == null) continue;
                            com.haskforce.haskell.psi.HaskellQconid last = Iterables.getLast(qconidList);
                            if (fullQualifierName.equals(last.getText())){
                                List<HaskellFile> filesByModuleName = HaskellModuleIndex.getFilesByModuleName(myElement.getProject(), moduleName.getText(), GlobalSearchScope.projectScope(myElement.getProject()));
                                List<PsiNamedElement> definitionNodes;
                                try {
                                    definitionNodes = HaskellUtil.findDefinitionNodes(filesByModuleName.get(0), functionName);
                                } catch (IndexOutOfBoundsException e) {
                                    continue;
                                }
                                for (PsiNamedElement definitionNode : definitionNodes) {
                                    String definitionNodeName = definitionNode.getName();
                                    if (definitionNodeName == null) continue;
                                    if (definitionNodeName.equals(functionName)){
                                        com.haskforce.haskell.psi.HaskellConid haskellConid;
                                        try {
                                            haskellConid = last.getConidList().get(i);
                                        } catch (IndexOutOfBoundsException e) {
                                            continue;
                                        }
                                        return new ResolveResult[]{new PsiElementResolveResult(haskellConid)};
                                    }
                                }
                            }
                        }

                    }

                } else {
                    if (myElement.getParent() instanceof com.haskforce.haskell.psi.HaskellTycon){
                        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
                        for (FoundDefinition namedElement : namedElements){
                            if (namedElement.element.getParent() instanceof com.haskforce.haskell.psi.HaskellTycon){
                                results.add(new PsiElementResolveResult(namedElement.element));
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
        if(PsiTreeUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellModuledecl.class) != null){
            return EMPTY_RESOLVE_RESULT;
        }


        com.haskforce.haskell.psi.HaskellImpdecl haskellImpdecl = PsiTreeUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellImpdecl.class);
        if (haskellImpdecl != null){
            PsiElement parent = myElement.getParent();
            if (parent instanceof com.haskforce.haskell.psi.HaskellQconid) {
                com.haskforce.haskell.psi.HaskellQconid haskellQconid = (com.haskforce.haskell.psi.HaskellQconid) parent;
                if (haskellImpdecl.getQconidList().get(0).equals(haskellQconid)) {
                    List<com.haskforce.haskell.psi.HaskellConid> conidList = haskellQconid.getConidList();
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

        String qualifiedCallName = HaskellUtil.getQualifiedPrefix(myElement);

        if (qualifiedCallName == null){
            /**
             * This is a set because sometimes there seems to be overlap between findDefintionNode which
             * should return all 'left-most' variables and the local variables. Also called
             *  a stop gap.
             */
            Set<PsiElement> resultSet = Sets.newHashSet();
            resultSet.addAll(HaskellUtil.matchGlobalNamesUnqualified(namedElements));

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
            results.addAll(HaskellUtil.matchGlobalNamesQualified(namedElements, qualifiedCallName));
            return results.toArray(new ResolveResult[results.size()]);
        }
    }

    private @NotNull List<PsiElementResolveResult> handleImportReferences(@NotNull com.haskforce.haskell.psi.HaskellImpdecl haskellImpdecl,
                                                                          @NotNull PsiNamedElement myElement, int i) {
        /**
         * Don't use the named element yet to determine which element we're
         * talking about, not necessary yet
         */
        List<PsiElementResolveResult> modulesFound = new ArrayList<PsiElementResolveResult>();
        List<com.haskforce.haskell.psi.HaskellQconid> qconidList = haskellImpdecl.getQconidList();
        if (qconidList.size() > 0){
            String moduleName = qconidList.get(0).getText();

            GlobalSearchScope globalSearchScope = GlobalSearchScope.projectScope(myElement.getProject());
            List<HaskellFile> filesByModuleName = HaskellModuleIndex.getFilesByModuleName(myElement.getProject(), moduleName, globalSearchScope);
            for (HaskellFile haskellFile : filesByModuleName) {
                com.haskforce.haskell.psi.HaskellModuledecl[] moduleDecls = PsiTreeUtil.getChildrenOfType(haskellFile, com.haskforce.haskell.psi.HaskellModuledecl.class);
                if (moduleDecls != null && moduleDecls.length > 0){
                    com.haskforce.haskell.psi.HaskellQconid qconid = moduleDecls[0].getQconid();
                    if (qconid != null) {
                        List<com.haskforce.haskell.psi.HaskellConid> conidList = moduleDecls[0].getQconid().getConidList();
                        modulesFound.add(new PsiElementResolveResult(conidList.get(i)));
                    }
                }
            }
        }
        return modulesFound;
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
        if (PsiTreeUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellExp.class) == null) {
            return new Object[]{};
        }
        // If we are in a qualified name, don't provide reference completion.
        final PsiElement qId = HaskellUtil.getParentOfType(myElement, com.haskforce.haskell.psi.HaskellQconid.class, com.haskforce.haskell.psi.HaskellQvarid.class);
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
            final PsiElement genDecl = PsiTreeUtil.getParentOfType(namedElement, com.haskforce.haskell.psi.HaskellGendecl.class);
            final PsiFile psiFile = namedElement.getContainingFile();
            if (!(psiFile instanceof HaskellFile)) { continue; }
            final String module = ((HaskellFile) psiFile).getModuleOrFileName();
            final String name = namedElement.getName();
            if (name == null) { continue; }
            final String type;
            if (genDecl != null) {
                final PsiElement cType = PsiTreeUtil.getChildOfType(genDecl, com.haskforce.haskell.psi.HaskellCtype.class);
                type = cType == null ? "" : cType.getText();
            } else {
                type = "";
            }
            variants.add(LookupElementUtil.create(name, module, type));
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
        if (myElement instanceof com.haskforce.haskell.psi.HaskellVarid) {
            element = HaskellPsiImplUtil.setName((com.haskforce.haskell.psi.HaskellVarid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        } else if (myElement instanceof com.haskforce.haskell.psi.HaskellConid) {
            element = HaskellPsiImplUtil.setName((com.haskforce.haskell.psi.HaskellConid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        }
        return super.handleElementRename(newName);
    }
}
