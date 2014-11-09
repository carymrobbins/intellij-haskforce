package com.haskforce.highlighting;

import com.haskforce.psi.*;
import com.haskforce.quickfixes.HaskellModuleFilenameFix;
import com.haskforce.quickfixes.HaskellModuleNameFix;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Annotator that:
 *
 * 1) brushes up syntax highlighting issues from parsing a broken program.
 * 2) Registers quickfixes on broken nodes.
 */
public class HaskellAnnotator implements Annotator {
    private static final String MSG = "File and module name differs";
    @Override
    public void annotate(@NotNull final PsiElement element, @NotNull final AnnotationHolder holder) {
        element.accept(new HaskellVisitor() {
            @Override
            public void visitPpragma(@NotNull HaskellPpragma o) {
                super.visitPpragma(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.PRAGMA);
            }

            @Override
            public void visitQvarid(@NotNull HaskellQvarid o) {
                super.visitQvarid(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.VARID);
            }

            @Override
            public void visitQvarsym(@NotNull HaskellQvarsym o) {
                super.visitQvarsym(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.VARSYM);
            }

            @Override
            public void visitQconsym(@NotNull HaskellQconsym o) {
                super.visitQconsym(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.CONSYM);
            }

            @Override
            public void visitQcon(@NotNull HaskellQcon o) {
                super.visitQcon(o);
                // Highlight the () unit type as a CONID if it's not in an import, e.g. `import Foo ()`.
                if (o.getText().equals("()")) {
                    final PsiElement prev1 = o.getPrevSibling();
                    final PsiElement prev2 = prev1 == null ? null : prev1.getPrevSibling();
                    final boolean inImport = prev1 instanceof HaskellImpdecl || prev2 instanceof HaskellImpdecl;
                    if (!inImport) {
                        setHighlighting(o, holder, HaskellSyntaxHighlighter.CONID);
                    }
                }
            }

            @Override
            public void visitPstringtoken(@NotNull HaskellPstringtoken o) {
                super.visitPstringtoken(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.STRING);
            }

            @Override
            public void visitShebang(@NotNull HaskellShebang o) {
                super.visitShebang(o);
                setHighlighting(o, holder, HaskellSyntaxHighlighter.COMMENT);
            }

            @Override
            public void visitModuledecl(@NotNull HaskellModuledecl o) {
                super.visitModuledecl(o);
                final HaskellQconid qc = o.getQconid();
                if (qc == null) {
                    return;
                }
                final List<HaskellConid> conidList = qc.getConidList();
                final HaskellConid lastConid = conidList.get(conidList.size() - 1);
                String moduleName = lastConid.getText();
                String fullFileName = o.getContainingFile().getName();
                //noinspection ConstantConditions
                if (fullFileName == null) {
                    return;
                }
                String fileSuffix = fullFileName.substring(fullFileName.lastIndexOf('.'));
                String fileName = fullFileName.substring(0, fullFileName.length() - fileSuffix.length());
                if (!moduleName.equals(fileName) && !"Main".equals(moduleName)) {
                    HaskellModuleFilenameFix fixFile = new HaskellModuleFilenameFix(moduleName + fileSuffix);
                    HaskellModuleNameFix fixName = new HaskellModuleNameFix(lastConid, fileName);
                    holder.createErrorAnnotation(qc, MSG).registerFix(fixFile);
                    holder.createErrorAnnotation(qc, MSG).registerFix(fixName);
                }
            }
        });
    }

    private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder,
                                        @NotNull TextAttributesKey key) {
        holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(
                EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
    }
}
