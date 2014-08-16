package com.haskforce.features.intentions;

import com.haskforce.psi.HaskellPpragma;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.FileASTNode;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public class IgnoreHLint extends BaseIntentionAction {
    public final String hint;

    public IgnoreHLint(String hint) {
        this.hint = hint;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Ignore all \"" + hint + "\" warnings";
    }

    @NotNull
    @Override
    public String getText() {
        return "Ignore all \"" + hint + "\" warnings";
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        // TODO: Add a setting for this.
        return true;
    }

    @Override
    public void invoke(@NotNull final Project project, Editor editor, final PsiFile file) throws IncorrectOperationException {
        PsiWhiteSpace newline = HaskellElementFactory.createNewLine(project);
        HaskellPpragma ppragma = HaskellElementFactory.createPpragmaFromText(
                project, "{-# ANN module (\"HLint: ignore " + hint + "\"::String) #-}");
        FileASTNode fileNode = file.getNode();

        // If the user has a module declaration, place the pragma after it.
        ASTNode node = fileNode.findChildByType(HaskellTypes.MODULEDECL);
        if (node != null) {
            file.addBefore(newline, file.addAfter(ppragma, node.getPsi()));
            return;
        }

        // If the user has any existing pragmas, place the pragma after them.
        ASTNode[] nodes = fileNode.getChildren(TokenSet.create(HaskellTypes.PPRAGMA));
        if (nodes.length > 0) {
            file.addBefore(newline, file.addAfter(ppragma, nodes[nodes.length - 1].getPsi()));
            return;
        }

        // Otherwise, just insert the pragma at the top of the file.
        file.addAfter(newline, file.addBefore(ppragma, file.getFirstChild()));
    }
}
