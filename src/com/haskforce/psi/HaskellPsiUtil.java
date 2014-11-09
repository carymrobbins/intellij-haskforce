package com.haskforce.psi;

import com.haskforce.utils.LogicUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class HaskellPsiUtil {
    @NotNull
    public static <T extends PsiElement> String[] getTexts(@NotNull List<T> psiElements) {
        final int size = psiElements.size();
        String[] result = new String[size];
        for (int i = 0; i < size; ++i) {
            result[i] = psiElements.get(i).getText();
        }
        return result;
    }

    @Nullable
    public static String parseModuleName(@NotNull PsiFile psiFile) {
        final HaskellModuledecl moduledecl = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
        if (moduledecl == null) { return null; }
        final HaskellQconid qconid = moduledecl.getQconid();
        if (qconid == null) { return null; }
        return moduledecl.getQconid().getText();
    }

    /**
     * Returns a map of module -> alias for each imported module.  If a module is imported but not qualified, alias
     * will be null.
     */
    @NotNull
    public static List<Import> parseImports(@NotNull final PsiFile file) {
        final Import prelude = Import.global("Prelude", new String[0], false);
        boolean importedPrelude = false;
        HaskellImpdecl[] impdecls = PsiTreeUtil.getChildrenOfType(PsiTreeUtil.getChildOfType(file, HaskellBody.class), HaskellImpdecl.class);
        // TODO: For now, just assume Prelude was implicitly imported if there are no import declarations.
        if (impdecls == null) { return Collections.singletonList(prelude); }
        List<Import> result = new ArrayList<Import>(impdecls.length);
        for (HaskellImpdecl impdecl : impdecls) {
            final List<HaskellQconid> qconids = impdecl.getQconidList();
            final int numQconids = qconids.size();
            if (numQconids == 0) { continue; }
            final HaskellQconid moduleQconid = qconids.get(0);
            final String module = moduleQconid.getText();
            final String alias = numQconids > 1 ? qconids.get(1).getText() : null;
            final PsiElement maybeQualified = PsiTreeUtil.prevVisibleLeaf(moduleQconid);
            final boolean isQualified = maybeQualified != null && maybeQualified.getText().equals("qualified");
            final PsiElement maybeHiding = PsiTreeUtil.nextVisibleLeaf(moduleQconid);
            final boolean hasHiding = maybeHiding != null && maybeHiding.getText().equals("hiding");
            //noinspection SuspiciousArrayCast
            final String[] nameList = getTexts(collectNamedElementsInImporttList(impdecl.getImporttList()));
            importedPrelude = importedPrelude || module.equals("Prelude");
            if (alias != null) {
                result.add(Import.qualifiedAs(module, alias, nameList, hasHiding));
                // If we have an alias but we didn't import qualified, we are also importing it globally.
                if (!isQualified) { result.add(Import.global(module, nameList, hasHiding)); }
            } else {
                result.add(isQualified ? Import.qualified(module, nameList, hasHiding) : Import.global(module, nameList, hasHiding));
            }
        }
        // TODO: Eventually we'll want to get fancy and check the cabal file and pragmas for NoImplicitPrelude.
        if (!importedPrelude) { result.add(prelude); }
        return result;
    }

    @NotNull
    public static List<PsiElement> getNamedElementsInImportt(HaskellImportt importt) {
        final List<PsiElement> result = new ArrayList<PsiElement>(importt.getChildren().length);
        importt.acceptChildren(new HaskellVisitor() {
            @Override
            public void visitCon(@NotNull HaskellCon o) {
                result.add(o);
            }

            @Override
            public void visitVarid(@NotNull HaskellVarid o) {
                result.add(o);
            }

            @Override
            public void visitVars(@NotNull HaskellVars o) {
                result.addAll(o.getVaridList());
            }

            @Override
            public void visitTycon(@NotNull HaskellTycon o) {
                result.add(o);
            }
        });
        return result;
    }

    @NotNull
    public static List<PsiElement> collectNamedElementsInImporttList(List<HaskellImportt> importts) {
        List<PsiElement> result = new ArrayList<PsiElement>(importts.size() * 2);
        for (HaskellImportt importt : importts) {
            result.addAll(getNamedElementsInImportt(importt));
        }
        return result;
    }

    public static class Import {
        public @NotNull final String module;
        public @Nullable final String alias;
        public @NotNull final String[] names;
        public @NotNull final String[] hiding;

        private Import(@NotNull String module, @Nullable String alias, @NotNull String[] names, boolean hasHiding) {
            this.module = module;
            this.alias = alias;
            if (hasHiding) {
                this.names = new String[0];
                this.hiding = names;
            } else {
                this.names = names;
                this.hiding = new String[0];
            }
        }

        public static Import global(@NotNull String module, @NotNull String[] names, boolean hasHiding) {
            return new Import(module, null, names, hasHiding);
        }

        public static Import qualified(@NotNull String module, @NotNull String[] names, boolean hasHiding) {
            return new Import(module, module, names, hasHiding);
        }

        public static Import qualifiedAs(@NotNull String module, @NotNull String alias, @NotNull String[] names, boolean hasHiding) {
            return new Import(module, alias, names, hasHiding);
        }

        public boolean isGlobal() {
            return alias == null;
        }

        public boolean isQualified() {
            return alias != null;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }

        @Override
        public boolean equals(Object o) {
            return EqualsBuilder.reflectionEquals(this, o);
        }

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this);
        }
    }
}
