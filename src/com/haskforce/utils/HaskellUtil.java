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
import com.intellij.util.ArrayUtil;
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
    public static List<FoundDefinition> findDefinitionNode(@NotNull Project project, @Nullable String name, @NotNull PsiNamedElement e) {
        // Guess where the name could be defined by lookup up potential modules.
        // TODO This removing duplicates, for example importing the same module twice. Fair enough
        final List<HaskellPsiUtil.Import> potentialModules =
                getPotentialDefinitionModuleNames(e, HaskellPsiUtil.parseImports(e.getContainingFile()));
        final Set<String> potentialModuleNames = new HashSet<String>();
        for (HaskellPsiUtil.Import i : potentialModules) {
            potentialModuleNames.add(i.module);
        }
        List<FoundDefinition> results = ContainerUtil.newArrayList();
        final String qPrefix = getQualifiedPrefix(e);
        final PsiFile psiFile = e.getContainingFile().getOriginalFile();
        if (psiFile instanceof HaskellFile) {
            List<PsiNamedElement> result = ContainerUtil.newArrayList();
            findDefinitionNode((HaskellFile)psiFile, name, e, result);
            addFoundDefinition(result, null, results);
        }
        for (HaskellPsiUtil.Import potentialModule : potentialModules) {
            List<PsiNamedElement> result = ContainerUtil.newArrayList();
            List<HaskellFile> files = HaskellModuleIndex.getFilesByModuleName(project, potentialModule.module, GlobalSearchScope.allScope(project));
            for (HaskellFile f : files) {
                final boolean returnAllReferences = name == null;
                final boolean inLocalModule = qPrefix == null && f.equals(psiFile);
                if (returnAllReferences || inLocalModule) {
                    findDefinitionNode(f, name, e, result);
                    findDefinitionNodeInExport(project, f, name, e, result);

                }
            }
            addFoundDefinition(result, potentialModule, results);
        }
        return results;
    }

    /**
     * Find definitions that have been re-exported.
     *
     * <code>
     *   module Foo (module Bar, foo) where
     *   import Bar
     *   import Baz (foo)
     * </code>
     */
    private static void findDefinitionNodeInExport(@NotNull Project project, HaskellFile f, @Nullable String name,
                                                   @Nullable PsiNamedElement e, List<PsiNamedElement> result) {
        List<HaskellPsiUtil.Import> imports = HaskellPsiUtil.parseImports(f);
        for (HaskellExport export : PsiTreeUtil.findChildrenOfType(f, HaskellExport.class)) {
            boolean exportFn = export.getQvar() != null && export.getQvar().getQvarid() != null
                    && export.getQvar().getQvarid().getVarid().getName().equals(name);
            String moduleName = exportFn
                    ? getModule(export.getQvar().getQvarid().getConidList())
                    : export.getModuletoken() != null && export.getQconid() != null ? export.getQconid().getText() : null;
            if (!exportFn && moduleName == null) continue;
            for (HaskellPsiUtil.Import imprt : imports) {
                if (moduleName != null && !moduleName.equals(imprt.module) && !moduleName.equals(imprt.alias)) continue;
                boolean hidden = imprt.getHidingNames() != null && ArrayUtil.contains(name, imprt.getHidingNames());
                boolean notImported = imprt.getImportedNames() != null && !ArrayUtil.contains(name, imprt.getImportedNames());
                if (hidden || notImported) continue;
                for (HaskellFile f2 : HaskellModuleIndex.getFilesByModuleName(project, imprt.module, GlobalSearchScope.allScope(project))) {
                    findDefinitionNode(f2, name, e, result);
                    findDefinitionNodeInExport(project, f2, name, e, result);
                }
            }
        }
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
        final boolean isType = PsiTreeUtil.getParentOfType(e, HaskellGendecl.class) != null;
        Collection<PsiNamedElement> namedElements = PsiTreeUtil.findChildrenOfType(file, elementClass);
        for (PsiNamedElement namedElement : namedElements) {
            if ((name == null || name.equals(namedElement.getName())) && definitionNode(namedElement)) {
                result.add(namedElement);
            } else if (isType && name != null && name.equals(namedElement.getName()) && typeNode(name, namedElement)) {
                result.add(namedElement);
            }
        }
    }

    private static boolean typeNode(@NotNull String name, @NotNull PsiNamedElement e) {
        HaskellDatadecl datadecl = PsiTreeUtil.getParentOfType(e, HaskellDatadecl.class);
        if (datadecl != null) {
            return datadecl.getTypeeList().get(0).getAtypeList().get(0).getText().equals(name);
        }
        HaskellNewtypedecl newtypedecl = PsiTreeUtil.getParentOfType(e, HaskellNewtypedecl.class);
        if (newtypedecl != null && newtypedecl.getTycon() != null) {
            return name.equals(newtypedecl.getTycon().getConid().getName());
        }
        HaskellTypedecl typedecl = PsiTreeUtil.getParentOfType(e, HaskellTypedecl.class);
        if (typedecl != null) {
            return name.equals(typedecl.getTypeeList().get(0).getAtypeList().get(0).getText());
        }
        HaskellClassdecl classdecl = PsiTreeUtil.getParentOfType(e, HaskellClassdecl.class);
        if (classdecl != null && classdecl.getCtype() != null) {
            HaskellCtype ctype = classdecl.getCtype();
            while (ctype.getCtype() != null) {
                ctype = ctype.getCtype();
            }
            if (ctype.getTypee() == null) return false;
            HaskellAtype haskellAtype = ctype.getTypee().getAtypeList().get(0);
            return haskellAtype.getOqtycon() != null && haskellAtype.getOqtycon().getQtycon() != null &&
                    name.equals(haskellAtype.getOqtycon().getQtycon().getTycon().getConid().getName());
        }
        return false;
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
     * Finds name definitions that are within the scope of a file, including imports (to some degree).
     */
    @NotNull
    public static List<PsiNamedElement> findDefinitionNodes(@NotNull HaskellFile psiFile) {
        return findDefinitionNodes(psiFile, null);
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
        final PsiElement q = getParentOfType(e, HaskellQcon.class, HaskellQvar.class);
        if (q == null) { return null; }
        final String qText = q.getText();
        final int lastDotPos = qText.lastIndexOf('.');
        if (lastDotPos == -1) { return null; }
        return qText.substring(0, lastDotPos);
    }

    /**
     * Helper method to avoid the compiler warning.
     * See https://youtrack.jetbrains.com/issue/IDEA-157225
     */
    @SafeVarargs
    @Nullable
    public static <T extends PsiElement> T getParentOfType(@Nullable final PsiElement element,
                                                           @NotNull final Class<? extends T>... classes) {
        return PsiTreeUtil.getParentOfType(element, classes);
    }

    @NotNull
    public static List<HaskellPsiUtil.Import> getPotentialDefinitionModuleNames(@NotNull PsiElement e, @NotNull List<HaskellPsiUtil.Import> imports) {
        final String qPrefix = getQualifiedPrefix(e);
        if (qPrefix == null) { return imports; }
        List<HaskellPsiUtil.Import> result = new ArrayList<HaskellPsiUtil.Import>(2);
        for (HaskellPsiUtil.Import anImport : imports) {
            if (qPrefix.equals(anImport.module) || qPrefix.equals(anImport.alias)) {
                result.add(anImport);
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
        List<HaskellVarid> varidList = new ArrayList<>(pat.getVaridList());
        for (HaskellPat haskellPat : pat.getPatList()) {
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
        HaskellGendecl haskellGendecl = PsiTreeUtil.getParentOfType(position, HaskellGendecl.class);
        return haskellGendecl != null;
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
            /**
             * This whole function needs to be re-evaluated, it's getting too much if,if,if. The logic
             * is getting extremely unclear. There should be tests for all (identified) cases so the refactor
             * should be feasible.
             */
            if (parent instanceof HaskellNewtypedecl){
                HaskellNewtypedecl haskellNewtypedecl = (HaskellNewtypedecl) parent;
                List<HaskellTyvar> tyvarList = haskellNewtypedecl.getTyvarList();
                for (HaskellTyvar haskellTyvar : tyvarList) {
                    HaskellVarid varId = haskellTyvar.getVarid();
                    if (varId.getName().matches(matcher)){
                        results.add(varId);
                    }
                }
            }
            PsiElement prevSibling = parent.getPrevSibling();
            while (prevSibling != null) {
                PsiElement possibleMatch = HaskellUtil.lookForFunOrPatDeclWithCorrectName(prevSibling, matcher);
                if (possibleMatch != null) {
                    results.add(possibleMatch);
                }
                if (prevSibling instanceof HaskellPat && parent instanceof HaskellExp) {
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

        } while(! (parent instanceof  PsiFile));

        return results;
    }


    public static List<PsiElement> matchGlobalNamesUnqualified(List<FoundDefinition> namedElements) {

        List<PsiElement> results = Lists.newArrayList();
        for (FoundDefinition possibleReferences : namedElements) {
            if (possibleReferences.imprt == null || !possibleReferences.imprt.isQualified) {
                //noinspection ObjectAllocationInLoop
                results.add(possibleReferences.element);
            }
        }
        return results;
    }


    public static List<PsiElementResolveResult> matchGlobalNamesQualified(
            List<FoundDefinition> namedElements, String qualifiedCallName){
        List<PsiElementResolveResult> results = Lists.newArrayList();
        for (FoundDefinition possibleReference : namedElements) {
            if(possibleReference.imprt != null && possibleReference.imprt.alias != null &&
                    possibleReference.imprt.alias.equals(qualifiedCallName)){
                results.add(new PsiElementResolveResult(possibleReference.element));
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

    private static void addFoundDefinition(List<PsiNamedElement> result, HaskellPsiUtil.Import imprt, List<FoundDefinition> results) {
        for (PsiNamedElement element : result) {
            results.add(new FoundDefinition(element, imprt));
        }
    }

    /**
     * Returns the textual representation of a qualified module.
     *
     * eg. From {@code A.B.C.d} return {@code A.B.C}
     */
    @Nullable
    private static String getModule(@NotNull List<HaskellConid> conids) {
        if (conids.isEmpty()) return null;
        StringBuilder b = new StringBuilder();
        for (HaskellConid cid : conids) {
            b.append(cid.getName());
            b.append(".");
        }
        b.setLength(b.length() - 1);
        return b.toString();
    }

    public static class FoundDefinition {
        @NotNull
        public PsiNamedElement element;

        @Nullable
        public HaskellPsiUtil.Import imprt;

        public FoundDefinition(@NotNull PsiNamedElement element, @Nullable HaskellPsiUtil.Import imprt) {
            this.element = element;
            this.imprt = imprt;
        }
    }
}
