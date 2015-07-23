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

    public CabalReference(PsiNamedElement element, TextRange range) {
        super(element, range);
    }

    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        return new ResolveResult[0];
    }

    @Nullable
    @Override
    public PsiElement resolve() {
        return null;
    }

    @NotNull
    @Override
    public Object[] getVariants() {
        return new Object[0];
    }
}
