package com.haskforce.utils;

import com.google.common.collect.Lists;
import com.haskforce.index.HaskellModuleIndex;
import com.haskforce.psi.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementResolveResult;
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
        // Now we have to figure out if the current varid, e, is the first top-level declaration in the file.
        // Check each top-level declaration.  When we find the first one that matches our element's name we'll return
        // true if the elements are equal, false otherwise.
        final String name = e.getName();
        final PsiFile file = e.getContainingFile();
        if (!(file instanceof HaskellFile)) return false;
        final HaskellBody body = ((HaskellFile)file).getBody();
        if (body == null) return false;
        for (PsiElement child  : body.getChildren()) {
            // If we hit a declaration with a type signature, this shouldn't match our element's name.
            if (child instanceof HaskellGendecl) {
                final HaskellVars vars = ((HaskellGendecl)child).getVars();
                if (vars == null) continue;
                // If it matches our elements name, return false.
                for (HaskellVarid varid : vars.getVaridList()) {
                    if (name.equals(varid.getName())) return false;
                }
            } else if (child instanceof HaskellFunorpatdecl) {
                final HaskellFunorpatdecl f = (HaskellFunorpatdecl)child;
                final HaskellVarop varop = f.getVarop();
                // Check if the function is defined as infix.
                if (varop != null) {
                    final HaskellVarid varid = varop.getVarid();
                    if (varid != null && name.equals(varid.getName())) {
                        return e.equals(varid);
                    }
                } else {
                    // If there is a pat in the declaration then there should only be one since the only case of having
                    // more than one is when using a varop, which was already accounted for above.
                    List<HaskellPat> pats = f.getPatList();
                    if (pats.size() == 1 && pats.get(0).getVaridList().contains(e)) return true;
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


    public static @Nullable PsiElement lookForFunOrPatDeclWithCorrectName(
            @NotNull PsiElement element,
            @NotNull String matcher){
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
                    PsiElement psiElement = checkForMatchingVariable(child,matcher);
                    if (psiElement != null){
                        return psiElement;
                    }
                }
                if (child instanceof HaskellPat){
                    HaskellPat pat = (HaskellPat)child;
                    List<HaskellVarid> varIds = extractAllHaskellVarids(pat);
                    for (HaskellVarid varId : varIds) {
                        if (varId.getName().matches(matcher)){
                            return varId;
                        };
                    }
                }
            }
        }
        return null;
    }

    public static List<HaskellVarid> extractAllHaskellVarids(HaskellPat pat) {
        List<HaskellVarid> varidList = pat.getVaridList();
        List<HaskellPat> patList = pat.getPatList();
        for (HaskellPat haskellPat : patList) {
            varidList.addAll(haskellPat.getVaridList());
        }
        return varidList;
    }

    private static PsiElement checkForMatchingVariable(PsiElement child, String matcher) {
        HaskellVarid haskellVarid = (HaskellVarid) child;
        if (haskellVarid.getName().matches(matcher)) {
            return child;
        } else {
            return null;
        }
    }

    public static boolean isInsideBody(@NotNull PsiElement position) {
        PsiElement parent = position.getParent();
        while(parent != null){
            if (parent instanceof HaskellBody){
                return true;
            }
            parent = parent.getParent();
        }
        return false;
    }

    public static @NotNull List<PsiElement> matchWhereClausesInScope(
            @NotNull PsiNamedElement myElement,
            String name) {
        return checkWhereClausesInScopeForVariableDeclaration(myElement, name);
    }

    public static @NotNull List<PsiElement> getAllDefinitionsInWhereClausesInScope(
            @NotNull PsiElement myElement) {
        return checkWhereClausesInScopeForVariableDeclaration(myElement, ".+");
    }

    private static @NotNull List<PsiElement> checkWhereClausesInScopeForVariableDeclaration(
                @NotNull PsiElement myElement,
                String matcher) {
        List<PsiElement>  results = Lists.newArrayList();
        PsiElement parent = myElement.getParent();
        do {
            if (parent instanceof HaskellRhs) {
                HaskellRhs rhs = (HaskellRhs) parent;
                PsiElement where = rhs.getWhere();
                if (where == null) {
                    parent = parent.getParent();
                    continue;
                } else {
                    PsiElement psiElement = checkWhereClause(where, matcher);
                    if (psiElement != null) {
                        results.add(psiElement);
                    }
                }
            }
            parent = parent.getParent();
        } while (! (parent instanceof  HaskellBody) && ! (parent == null));

        return results;
    }

    private static @Nullable PsiElement checkWhereClause(@NotNull PsiElement where, String matcher) {
        PsiElement nextSibling = where.getNextSibling();
        while(nextSibling != null){
            if(nextSibling instanceof HaskellFunorpatdecl) {
                PsiElement psiElement = HaskellUtil.lookForFunOrPatDeclWithCorrectName(nextSibling, matcher);
                if (psiElement != null){
                    return psiElement;
                }
            }
            nextSibling = nextSibling.getNextSibling();
        }
        return null;
    }

    public static @NotNull List<PsiElement> matchLocalDefinitionsInScope(PsiElement element, String name){
        return checkLocalDefinitionsForVariableDeclarations(element,name);
    }

    public static @NotNull List<PsiElement> getAllDefinitionsInScope(PsiElement element){
        return checkLocalDefinitionsForVariableDeclarations(element,".+");
    }

    private static @NotNull List<PsiElement> checkLocalDefinitionsForVariableDeclarations(PsiElement element, String matcher){
        List<PsiElement> results = Lists.newArrayList();
        PsiElement parent = element;
        do {

            PsiElement prevSibling = parent.getPrevSibling();
            while (prevSibling != null) {
                PsiElement possibleMatch = HaskellUtil.lookForFunOrPatDeclWithCorrectName(prevSibling, matcher);
                if (possibleMatch != null) {
                    results.add(possibleMatch);
                }
                if (prevSibling instanceof HaskellPat) {
                    List<HaskellVarid> varIds = HaskellUtil.extractAllHaskellVarids((HaskellPat) prevSibling);
                    for (HaskellVarid varId : varIds) {
                        if (varId.getName().matches(matcher)) {
                            results.add(varId);
                        }
                    }
                }
                if (prevSibling instanceof HaskellVarid){
                    HaskellVarid varId = (HaskellVarid) prevSibling;
                    if (varId.getName().matches(matcher)){
                        results.add(varId);
                    }
                }
                prevSibling = prevSibling.getPrevSibling();
            }
            parent = parent.getParent();
            /**This is only necessary to put the caret on the declaration when Go to symbol is called
             * when the caret is on the declaration. instead of saying that it didn't find a declaration.
             * Might be that this is not that necessary (and it's once more over the tree). If it's necessary,
             * a saner implementation will be to just first check whether the caret is on a declaration and leave it there.
             * But don't yet know whether this is necessary or not.
             */
//            PsiElement psiElement = lookForFunOrPatDeclWithCorrectName(parent);
//            if (psiElement != null) {
//                return psiElement;
//            }

        } while(! (parent instanceof  PsiFile));

        return results;
    }

/*    public static @NotNull String extractQualifierPrefix(@NotNull PsiElement element) {
        PsiElement parent = element.getParent();
        if (parent instanceof HaskellQvarid){
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
        return "";
    }*/

    public static List<PsiElementResolveResult> matchGlobalNamesUnqualified(
            PsiElement psiElement,
            List<PsiNamedElement> namedElements,
            List<HaskellImpdecl> importDeclarations){

        String ownModuleName = getModuleName(psiElement);
        List<PsiElementResolveResult> results = Lists.newArrayList();
        for (PsiNamedElement property : namedElements) {
            String moduleName = getModuleName(property);
            if (importPresent(moduleName, importDeclarations) || ownModuleName.equals(moduleName)) {
                //noinspection ObjectAllocationInLoop
                results.add(new PsiElementResolveResult(property));
            }
        }
        return results;
    }


    public static List<PsiElementResolveResult> matchGlobalNamesQualified(
            List<PsiNamedElement> namedElements, String qualifiedCallName,
            List<HaskellImpdecl> importDeclarations){
        List<PsiElementResolveResult> results = Lists.newArrayList();
        for (PsiNamedElement property : namedElements) {
            String moduleName = getModuleName(property);
            HaskellImpdecl correspondingImportDeclaration =
                    findCorrespondingImportDeclaration(qualifiedCallName, importDeclarations);
            if(correspondingImportDeclaration != null &&
                    namedElementDefinedInCorrespondingModule(correspondingImportDeclaration,moduleName)){
                results.add(new PsiElementResolveResult(property));
            }
        }
        return results;

    }

    public static @NotNull String getModuleName(@NotNull PsiElement element) {
        HaskellFile containingFile = (HaskellFile)element.getContainingFile();
        if (containingFile == null){
            return "";
        }
        String moduleName = containingFile.getModuleName();
        if(moduleName != null){
            return moduleName;
        } else {
            return "";
        }
    }


    /**
     * Not going to do it like this, going to this once and create a map of or so
     * of imported modules, going to make the look up a bit more cpu friendly.
     * For now this works.
     */
    public static boolean importPresent(@NotNull String moduleName, @NotNull List<HaskellImpdecl> importDeclarations) {
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

    private static boolean namedElementDefinedInCorrespondingModule(HaskellImpdecl correspondingImportDeclaration, String moduleName) {
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

    /**
     * This might have to move to the haskellImplDecl itself
     * @param importDeclaration
     */
    private static @Nullable String getQualifiedImportName(HaskellImpdecl importDeclaration){
        List<HaskellQconid> qconidList = importDeclaration.getQconidList();

        PsiElement as = importDeclaration.getAs();
        HaskellQconid haskellQconid = null;
        if (as != null) {
            if(qconidList.size()<2){
                return null;
            }
            haskellQconid = qconidList.get(1);
        } else {
            if (qconidList.size()<1){
                return null;
            }
            haskellQconid = qconidList.get(0);
        }

        StringBuilder qualifiedImportBuilder = new StringBuilder();
             for (HaskellConid haskellConid : haskellQconid.getConidList()) {
            qualifiedImportBuilder.append(haskellConid.getName());
            qualifiedImportBuilder.append('.');
        }
        qualifiedImportBuilder.deleteCharAt(qualifiedImportBuilder.length() - 1);

        return qualifiedImportBuilder.toString();
    }

    private static HaskellImpdecl findCorrespondingImportDeclaration(String qualifiedCallName, List<HaskellImpdecl> importDeclarations) {
        for (HaskellImpdecl importDeclaration : importDeclarations) {
            String qualifiedImportName = getQualifiedImportName(importDeclaration);
            if (qualifiedImportName != null && qualifiedCallName.equals(qualifiedImportName)){
                return importDeclaration;
            }
        }
        return null;
    }
}
