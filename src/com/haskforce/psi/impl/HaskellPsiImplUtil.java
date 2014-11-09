package com.haskforce.psi.impl;

import com.haskforce.HaskellIcons;
import com.haskforce.psi.HaskellPsiUtil;
import com.haskforce.psi.references.HaskellReference;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellVarid;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * Source of the methods pointed out in Haskell.bnf.
 */
public class HaskellPsiImplUtil {

    @Nullable
    public static String getName(@NotNull HaskellVarid o) {
        PsiElement n = o.getVaridRegexp();
        return n == null ? null : n.getText();
    }

    @NotNull
    public static String getName(@NotNull HaskellConid o) {
        return o.getConidRegexp().getText();
    }

    @Nullable
    public static PsiElement getNameIdentifier(@NotNull HaskellVarid o) {
        ASTNode keyNode = o.getNode();
        return keyNode != null ? keyNode.getPsi() : null;
    }

    @Nullable
    public static PsiElement getNameIdentifier(@NotNull HaskellConid o) {
        ASTNode keyNode = o.getNode();
        return keyNode != null ? keyNode.getPsi() : null;
    }

    @Nullable
    public static PsiElement setName(@NotNull HaskellVarid o, @NotNull String newName) {
        PsiElement e = HaskellElementFactory.createVaridFromText(o.getProject(), newName);
        if (e == null) return null;
        o.replace(e);
        return o;
    }

    @Nullable
    public static PsiElement setName(@NotNull HaskellConid o, @NotNull String newName) {
        PsiElement e = HaskellElementFactory.createConidFromText(o.getProject(), newName);
        if (e == null) return null;
        o.replace(e);
        return o;
    }

    @NotNull
    public static PsiReference getReference(@NotNull HaskellVarid o) {
        String s = getName(o);
        return new HaskellReference(o, TextRange.from(0, s == null ? 0 : s.length()));
    }

    @NotNull
    public static PsiReference getReference(@NotNull HaskellConid o) {
        return new HaskellReference(o, TextRange.from(0, getName(o).length()));
    }

    // Used for go to symbol.
    @NotNull
    public static ItemPresentation getPresentation(final HaskellVarid o) {
        return new ItemPresentation() {
            @Nullable
            @Override
            public String getPresentableText() {
                return o.getName();
            }

            /**
             * This is needed to decipher between files when resolving multiple references.
             */
            @Nullable
            @Override
            public String getLocationString() {
                return HaskellPsiUtil.getModuleOrFileName(o.getContainingFile());
            }

            @Nullable
            @Override
            public Icon getIcon(boolean unused) {
                return HaskellIcons.FILE;
            }
        };
    }

    @NotNull
    public static ItemPresentation getPresentation(final HaskellConid o) {
        return new ItemPresentation() {
            @Nullable
            @Override
            public String getPresentableText() {
                return o.getName();
            }

            /**
             * This is needed to decipher between files when resolving multiple references.
             */
            @Nullable
            @Override
            public String getLocationString() {
                return HaskellPsiUtil.getModuleOrFileName(o.getContainingFile());
            }

            @Nullable
            @Override
            public Icon getIcon(boolean unused) {
                return HaskellIcons.FILE;
            }
        };
    }
}
