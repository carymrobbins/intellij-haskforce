package com.haskforce.haskell.psi;

import com.intellij.openapi.util.Condition;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

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

    public static boolean hasPragma(@NotNull PsiFile file, @NotNull String pragmaName) {
        com.haskforce.haskell.psi.HaskellPpragma[] ppragmas = PsiTreeUtil.getChildrenOfType(file, com.haskforce.haskell.psi.HaskellPpragma.class);
        if (ppragmas == null) return false;
        for (com.haskforce.haskell.psi.HaskellPpragma ppragma : ppragmas) {
            if (ppragma.getText().contains(pragmaName)) return true;
        }
        return false;
    }

    /**
     * Returns a map of module -> alias for each imported module.  If a module is imported but not qualified, alias
     * will be null.
     */
    @NotNull
    public static List<Import> parseImports(@NotNull final PsiFile file) {
        final boolean noImplicitPrelude = hasPragma(file, "NoImplicitPrelude");
        final Import prelude = Import.global("Prelude", false, null);
        boolean importedPrelude = false;
        com.haskforce.haskell.psi.HaskellImpdecl[] impdecls = PsiTreeUtil.getChildrenOfType(PsiTreeUtil.getChildOfType(file, com.haskforce.haskell.psi.HaskellBody.class), com.haskforce.haskell.psi.HaskellImpdecl.class);
        if (impdecls == null) {
            if (noImplicitPrelude) return Collections.emptyList();
            return Collections.singletonList(prelude);
        }
        List<Import> result = new ArrayList<Import>(impdecls.length);
        for (com.haskforce.haskell.psi.HaskellImpdecl impdecl : impdecls) {
            final List<com.haskforce.haskell.psi.HaskellQconid> qconids = impdecl.getQconidList();
            final int numQconids = qconids.size();
            if (numQconids == 0) { continue; }
            final com.haskforce.haskell.psi.HaskellQconid moduleQconid = qconids.get(0);
            final String module = moduleQconid.getText();
            final String alias = numQconids > 1 ? qconids.get(1).getText() : null;
            final boolean isQualified = impdecl.getQualified() != null;
            final boolean isHiding = impdecl.getHiding() != null;
            final String[] explicitNames;
            // Check if we have an empty import list.
            if (impdecl.getImpempty() != null) {
                explicitNames = ArrayUtils.EMPTY_STRING_ARRAY;
            // Otherwise, if we have a left paren, we have an import list.
            } else if (impdecl.getLparen() != null) {
                explicitNames = getTexts(collectNamedElementsInImporttList(impdecl.getImporttList()));
            // At this point, we must not have an import list at all.
            } else {
                explicitNames = null;
            }
            importedPrelude = importedPrelude || module.equals("Prelude");
            final Import anImport;
            if (isQualified) {
                if (alias == null) {
                    anImport = Import.qualified(module, isHiding, explicitNames);
                } else {
                    anImport = Import.qualifiedAs(module, alias, isHiding, explicitNames);
                }
            } else {
                if (alias == null) {
                    anImport = Import.global(module, isHiding, explicitNames);
                } else {
                    anImport = Import.globalAs(module, alias, isHiding, explicitNames);
                }
            }
            result.add(anImport);
        }
        if (!importedPrelude && !noImplicitPrelude) { result.add(prelude); }
        return result;
    }

    public static boolean isType(@Nullable PsiElement e, @NotNull IElementType t) {
        return e != null && e.getNode().getElementType().equals(t);
    }

    @NotNull
    public static List<PsiElement> getNamedElementsInImportt(com.haskforce.haskell.psi.HaskellImportt importt) {
        final List<PsiElement> result = new ArrayList<PsiElement>(importt.getChildren().length);
        importt.acceptChildren(new com.haskforce.haskell.psi.HaskellVisitor() {
            @Override
            public void visitCon(@NotNull com.haskforce.haskell.psi.HaskellCon o) {
                result.add(o);
            }

            @Override
            public void visitVarid(@NotNull com.haskforce.haskell.psi.HaskellVarid o) {
                result.add(o);
            }

            @Override
            public void visitVars(@NotNull com.haskforce.haskell.psi.HaskellVars o) {
                result.addAll(o.getVaridList());
            }

            @Override
            public void visitTycon(@NotNull com.haskforce.haskell.psi.HaskellTycon o) {
                result.add(o);
            }
        });
        return result;
    }

    @NotNull
    public static List<PsiElement> collectNamedElementsInImporttList(List<com.haskforce.haskell.psi.HaskellImportt> importts) {
        List<PsiElement> result = new ArrayList<PsiElement>(importts.size() * 2);
        for (com.haskforce.haskell.psi.HaskellImportt importt : importts) {
            result.addAll(getNamedElementsInImportt(importt));
        }
        return result;
    }

    @NotNull
    public static Set<String> getImportModuleNames(@NotNull List<Import> imports) {
        return ContainerUtil.map2Set(imports, new Function<Import, String>() {
            @Override
            public String fun(Import anImport) {
                return anImport.module;
            }
        });
    }

    public static class Import {
        public final boolean isQualified;
        public @NotNull final String module;
        public @Nullable final String alias;
        public final boolean isHiding;
        private @Nullable final String[] explicitNames;

        private Import(boolean isQualified, @NotNull String module, @Nullable String alias, boolean isHiding, @Nullable String[] explicitNames) {
            this.isQualified = isQualified;
            this.module = module;
            this.alias = alias;
            this.isHiding = isHiding;
            this.explicitNames = explicitNames;
        }

        public static Import global(@NotNull String module, boolean isHiding, @Nullable String[] explicitNames) {
            return new Import(false, module, null, isHiding, explicitNames);
        }

        public static Import globalAs(@NotNull String module, @NotNull String alias, boolean isHiding, @Nullable String[] explicitNames) {
            return new Import(false, module, alias, isHiding, explicitNames);
        }

        public static Import qualified(@NotNull String module, boolean isHiding, @Nullable String[] explicitNames) {
            return new Import(true, module, module, isHiding, explicitNames);
        }

        public static Import qualifiedAs(@NotNull String module, @NotNull String alias, boolean isHiding, @Nullable String[] explicitNames) {
            return new Import(true, module, alias, isHiding, explicitNames);
        }

        @Nullable
        public String[] getImportedNames() {
            return isHiding ? null : explicitNames;
        }

        @Nullable
        public String[] getHidingNames() {
            return isHiding ? explicitNames : null;
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
