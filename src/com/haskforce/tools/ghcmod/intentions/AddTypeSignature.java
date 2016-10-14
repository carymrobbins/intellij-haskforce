package com.haskforce.tools.ghcmod.intentions;

import com.haskforce.tools.ghcmod.mod.GhcMod.*;
import com.haskforce.haskell.psi.HaskellBody;
import com.haskforce.haskell.psi.HaskellFunorpatdecl;
import com.haskforce.haskell.psi.HaskellGendecl;
import com.haskforce.haskell.psi.impl.HaskellElementFactory;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

//with the current level of abstractions i don't see how this class could move to the haskell package
//(it would probably produce much boilerplate and little gain with an nearly emtpy class)
public class AddTypeSignature extends BaseIntentionAction {
    public final Problem problem;

    public AddTypeSignature(Problem problem) {
        this.problem = problem;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Add type signature";
    }

    @NotNull
    @Override
    public String getText() {
        return "Add type signature";
    }

    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        // TODO: Add a setting for this.
        return true;
    }

    private static final Pattern COLON_REGEX = Pattern.compile(":");
    private static final Pattern CHAR_LIST_REGEX = Pattern.compile("\\[Char]");

    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
        final String[] messageParts = COLON_REGEX.split(problem.message, 2);
        if (messageParts.length != 2) { return; }
        // Substitute `String` for `[Char]` in type signatures.
        final String typeSignature = CHAR_LIST_REGEX.matcher(messageParts[1].trim()).replaceAll("String");
        final PsiElement currentElement = file.findElementAt(editor.getCaretModel().getOffset());
        final HaskellFunorpatdecl funorpatdecl = PsiTreeUtil.getParentOfType(currentElement, HaskellFunorpatdecl.class);
        final HaskellBody body = PsiTreeUtil.getParentOfType(funorpatdecl, HaskellBody.class);
        if (body != null) {
            final HaskellGendecl gendecl = HaskellElementFactory.createGendeclFromText(project, typeSignature);
            final PsiWhiteSpace newline = HaskellElementFactory.createNewLine(project);
            body.addBefore(gendecl, funorpatdecl);
            body.addBefore(newline, funorpatdecl);
        }
    }
}
