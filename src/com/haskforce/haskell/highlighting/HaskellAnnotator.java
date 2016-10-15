package com.haskforce.haskell.highlighting;

import com.haskforce.haskell.psi.*;
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
        element.accept(new com.haskforce.haskell.psi.HaskellVisitor() {
            @Override
            public void visitPpragma(@NotNull com.haskforce.haskell.psi.HaskellPpragma o) {
                super.visitPpragma(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.PRAGMA);
            }

            @Override
            public void visitQvarid(@NotNull com.haskforce.haskell.psi.HaskellQvarid o) {
                super.visitQvarid(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.VARID);
            }

            @Override
            public void visitQvarsym(@NotNull com.haskforce.haskell.psi.HaskellQvarsym o) {
                super.visitQvarsym(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.VARSYM);
            }

            @Override
            public void visitQconsym(@NotNull com.haskforce.haskell.psi.HaskellQconsym o) {
                super.visitQconsym(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.CONSYM);
            }

            @Override
            public void visitQcon(@NotNull com.haskforce.haskell.psi.HaskellQcon o) {
                super.visitQcon(o);
                // Highlight the () unit type as a CONID if it's not in an import, e.g. `import Foo ()`.
                if (o.getText().equals("()")) {
                    final PsiElement prev1 = o.getPrevSibling();
                    final PsiElement prev2 = prev1 == null ? null : prev1.getPrevSibling();
                    final boolean inImport = prev1 instanceof com.haskforce.haskell.psi.HaskellImpdecl || prev2 instanceof com.haskforce.haskell.psi.HaskellImpdecl;
                    if (!inImport) {
                        setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.CONID);
                    }
                }
            }

            @Override
            public void visitAtype(@NotNull com.haskforce.haskell.psi.HaskellAtype o) {
                super.visitAtype(o);
                // Highlight the () unit type as a CONID.
                if (o.getText().equals("()")) {
                    setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.CONID);
                }
            }

            @Override
            public void visitPstringtoken(@NotNull com.haskforce.haskell.psi.HaskellPstringtoken o) {
                super.visitPstringtoken(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.STRING);
            }

            @Override
            public void visitShebang(@NotNull com.haskforce.haskell.psi.HaskellShebang o) {
                super.visitShebang(o);
                setHighlighting(o, holder, com.haskforce.haskell.highlighting.HaskellSyntaxHighlighter.COMMENT);
            }

            @Override
            public void visitModuledecl(@NotNull com.haskforce.haskell.psi.HaskellModuledecl o) {
                super.visitModuledecl(o);
                final com.haskforce.haskell.psi.HaskellQconid qc = o.getQconid();
                if (qc == null) {
                    return;
                }
                final List<com.haskforce.haskell.psi.HaskellConid> conidList = qc.getConidList();
                final com.haskforce.haskell.psi.HaskellConid lastConid = conidList.get(conidList.size() - 1);
                String moduleName = lastConid.getText();
                String fullFileName = o.getContainingFile().getName();
                //noinspection ConstantConditions
                if (fullFileName == null) {
                    return;
                }
                String fileSuffix = fullFileName.substring(fullFileName.lastIndexOf('.'));
                String fileName = fullFileName.substring(0, fullFileName.length() - fileSuffix.length());
                if (!moduleName.equals(fileName) && !"Main".equals(moduleName)) {
                    com.haskforce.haskell.quickfixes.HaskellModuleFilenameFix fixFile = new com.haskforce.haskell.quickfixes.HaskellModuleFilenameFix(moduleName + fileSuffix);
                    com.haskforce.haskell.quickfixes.HaskellModuleNameFix fixName = new com.haskforce.haskell.quickfixes.HaskellModuleNameFix(lastConid, fileName);
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
