package com.haskforce.psi.impl;

import com.haskforce.HaskellIcons;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellQqblob;
import com.haskforce.psi.references.HaskellReference;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellVarid;
import com.haskforce.stubs.HaskellConidStub;
import com.haskforce.stubs.HaskellVaridStub;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * Source of the methods pointed out in Haskell.bnf.
 */
public class HaskellPsiImplUtil {

    @NotNull
    public static String getName(@NotNull HaskellVarid o) {
        HaskellVaridStub stub = o.getStub();
        if (stub != null) return StringUtil.notNullize(stub.getName());
        return o.getText();
    }

    @NotNull
    public static String getName(@NotNull HaskellConid o) {
        HaskellConidStub stub = o.getStub();
        if (stub != null) return StringUtil.notNullize(stub.getName());
        return o.getText();
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

    public static boolean isValidHost(@NotNull HaskellQqblob o) {
        return true;
    }

    public static HaskellQqblob updateText(@NotNull HaskellQqblob o, @NotNull String s) {
        final ASTNode valueNode = o.getNode().getFirstChildNode();
        assert valueNode instanceof LeafElement;
        ((LeafElement) valueNode).replaceWithText(s);
        return o;
    }

    @NotNull
    public static StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper(@NotNull HaskellQqblob o) {
        return new StringLiteralEscaper<HaskellQqblob>(o);
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
                final PsiFile psiFile = o.getContainingFile();
                return psiFile instanceof HaskellFile ? ((HaskellFile) psiFile).getModuleOrFileName() : null;
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
                final PsiFile psiFile = o.getContainingFile();
                return psiFile instanceof HaskellFile ? ((HaskellFile) psiFile).getModuleOrFileName() : null;
            }

            @Nullable
            @Override
            public Icon getIcon(boolean unused) {
                return HaskellIcons.FILE;
            }
        };
    }
}
