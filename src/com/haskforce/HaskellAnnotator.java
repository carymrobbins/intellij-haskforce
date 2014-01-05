package com.haskforce;

import com.haskforce.psi.*;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class HaskellAnnotator implements Annotator {
    @Override
    public void annotate(@NotNull final PsiElement element, @NotNull final AnnotationHolder holder) {
        element.accept(new HaskellVisitor() {
            @Override
            public void visitStringtoken(@NotNull HaskellStringtoken o) {
                super.visitStringtoken(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.STRING);
            }

            @Override
            public void visitIntegertoken(@NotNull HaskellIntegertoken o) {
                super.visitIntegertoken(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.INTEGER);
            }

            @Override
            public void visitFloattoken(@NotNull HaskellFloattoken o) {
                super.visitFloattoken(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.FLOAT);
            }

            @Override
            public void visitChartoken(@NotNull HaskellChartoken o) {
                super.visitChartoken(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.CHAR);
            }

            @Override
            public void visitSpecial(@NotNull HaskellSpecial o) {
                super.visitSpecial(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.SPECIAL);
            }

            @Override
            public void visitNcomment(@NotNull HaskellNcomment o) {
                super.visitNcomment(o);
                setHighlightingRecursive(o, holder, HaskellSyntaxHighlighter.NCOMMENT);
            }

            @Override
            public void visitVarid(@NotNull HaskellVarid o) {
                super.visitVarid(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.VARID);
            }

            @Override
            public void visitReservedid(@NotNull HaskellReservedid o) {
                super.visitReservedid(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.RESERVEDID);
            }

            @Override
            public void visitVarsym(@NotNull HaskellVarsym o) {
                super.visitVarsym(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.VARSYM);
            }

            @Override
            public void visitConsym(@NotNull HaskellConsym o) {
                super.visitConsym(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.CONSYM);
            }

            @Override
            public void visitReservedop(@NotNull HaskellReservedop o) {
                super.visitReservedop(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.RESERVEDOP);
            }

            @Override
            public void visitModid(@NotNull HaskellModid o) {
                super.visitModid(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.MODULE);
            }

            @Override
            public void visitEscape(@NotNull HaskellEscape o) {
                super.visitEscape(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.ESCAPE);
            }
        });
    }

    private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder,
                                        @NotNull TextAttributesKey key) {
        holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(TextAttributes.ERASE_MARKER);
        holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(
                EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
    }

    private static void setHighlightingRecursive(@NotNull PsiElement element, @NotNull AnnotationHolder holder,
                                                 @NotNull TextAttributesKey key) {
        setHighlighting(element, holder, key);
        for (PsiElement child : element.getChildren()) {
            setHighlightingRecursive(child, holder, key);
        }
    }
}
