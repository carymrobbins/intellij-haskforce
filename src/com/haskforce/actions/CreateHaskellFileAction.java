package com.haskforce.actions;

import com.haskforce.HaskellIcons;
import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.InputValidatorEx;
import com.intellij.psi.PsiDirectory;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.generate.tostring.util.StringUtil;

public class CreateHaskellFileAction extends CreateFileFromTemplateAction implements DumbAware {
    private static final String NEW_HASKELL_FILE = "New Haskell File";

    public CreateHaskellFileAction() {
        super(NEW_HASKELL_FILE, "", HaskellIcons.FILE);
    }

    @Override
    protected void buildDialog(Project project, PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder) {
        builder.setTitle(NEW_HASKELL_FILE)
               .addKind("Empty module", HaskellIcons.FILE, "Haskell Module")
               .setValidator(new InputValidatorEx() {
                   @Nullable
                   @Override
                   public String getErrorText(String inputString) {
                       String error = " is not a valid Haskell module name.";
                       if (StringUtil.isEmpty(inputString)) {
                           return null;
                       }
                       if (inputString.matches("^[A-Z][A-Za-z0-9]*$")) {
                           return null;
                       }
                       return "'" + inputString + "'" + error;
                   }

                   @Override
                   public boolean checkInput(String inputString) {
                       return true;
                   }

                   @Override
                   public boolean canClose(String inputString) {
                       return getErrorText(inputString) == null;
                   }
               });
    }

    @Override
    protected String getActionName(PsiDirectory directory, String newName, String templateName) {
        return NEW_HASKELL_FILE;
    }
}
