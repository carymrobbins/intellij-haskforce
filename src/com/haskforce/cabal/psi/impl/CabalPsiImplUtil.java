package com.haskforce.cabal.psi.impl;

import com.haskforce.cabal.psi.CabalVarid;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.haskforce.psi.references.CabalReference;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class CabalPsiImplUtil {


    @NotNull
    public static String getName(@NotNull CabalVarid o) {
        return o.getText();
    }

    @NotNull
    public static PsiReference getReference(@NotNull CabalVarid o) {
        String s = getName(o);
        return new CabalReference(o, TextRange.from(0, s.length()));
    }


    @Nullable
    public static PsiElement getNameIdentifier(@NotNull CabalVarid o) {
        ASTNode keyNode = o.getNode();
        return keyNode != null ? keyNode.getPsi() : null;
    }


    @Nullable
    public static PsiElement setName(@NotNull CabalVarid o, @NotNull String newName) {
        PsiElement e = CabalElementFactory.createVaridFromText (o.getProject(), newName);
        if (e == null) return null;
        o.replace(e);
        return o;
    }



}
