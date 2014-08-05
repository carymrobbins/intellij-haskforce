package com.haskforce.actions;

import com.haskforce.HaskellIcons;
import com.haskforce.utils.FileUtil;
import com.intellij.ide.actions.CreateFileAction;
import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.ide.fileTemplates.FileTemplate;
import com.intellij.ide.fileTemplates.FileTemplateManager;
import com.intellij.ide.fileTemplates.FileTemplateUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.InputValidatorEx;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.ParseException;
import java.util.List;
import java.util.Properties;
import java.util.regex.Pattern;

public class CreateHaskellFileAction extends CreateFileFromTemplateAction implements DumbAware {
    private static final String NEW_HASKELL_FILE = "New Haskell File";
    private static final Pattern VALID_MODULE_NAME_REGEX = Pattern.compile("^[A-Z][A-Za-z0-9]*(\\.hs)?$");

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
                       final String error = " is not a valid Haskell module name.";
                       if (inputString.isEmpty()) {
                           return null;
                       }
                       if (VALID_MODULE_NAME_REGEX.matcher(inputString).matches()) {
                           return null;
                       }
                       return '\'' + inputString + '\'' + error;
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
    protected PsiFile createFileFromTemplate(String name, FileTemplate template, PsiDirectory dir) {
        // Strip extension so we don't end up with a file saved as "Foo.hs.hs" and content of `module Foo.hs where`
        if (name.endsWith(".hs")) {
            name = name.substring(0, name.lastIndexOf('.'));
        }
        return createFileFromTemplate(name, template, dir, getDefaultTemplateProperty());
    }

    @SuppressWarnings("DialogTitleCapitalization")
    @Nullable
    public static PsiFile createFileFromTemplate(@Nullable String name,
                                                 @NotNull FileTemplate template,
                                                 @NotNull PsiDirectory dir,
                                                 @Nullable String defaultTemplateProperty) {
        // TODO: Do we *have* to hack the IntelliJ source?
        // This is a roughly a copy/paste then slight adaptation from the IntelliJ definition of this method.
        // We can't override it directly, and more importantly we can't override its call to
        // FileTemplateUtil.createFromTemplate()
        List<String> pathItems = FileUtil.getPathFromSourceRoot(dir.getProject(), dir.getVirtualFile());
        final String modulePrefix = pathItems == null ? "" : StringUtil.join(pathItems, ".");

        // Adapted from super definition.
        CreateFileAction.MkDirs mkdirs = new CreateFileAction.MkDirs(name, dir);
        name = mkdirs.newName;
        dir = mkdirs.directory;
        PsiElement element;
        Project project = dir.getProject();
        try {
            // Patch props with custom property.
            Properties props = FileTemplateManager.getInstance().getDefaultProperties(project);
            props.setProperty("HASKELL_MODULE_NAME", modulePrefix.isEmpty() || name == null ? name : modulePrefix + '.' + name);
            element = FileTemplateUtil
                    .createFromTemplate(template, name, props, dir);

            final PsiFile psiFile = element.getContainingFile();

            final VirtualFile virtualFile = psiFile.getVirtualFile();
            if (virtualFile != null) {
                FileEditorManager.getInstance(project).openFile(virtualFile, true);
                if (defaultTemplateProperty != null) {
                    PropertiesComponent.getInstance(project).setValue(defaultTemplateProperty, template.getName());
                }
                return psiFile;
            }
        }
        catch (ParseException e) {
            Messages.showErrorDialog(project, "Error parsing Velocity template: " + e.getMessage(), "Create File from Template");
            return null;
        }
        catch (IncorrectOperationException e) {
            throw e;
        }
        catch (Exception e) {
            LOG.error(e);
        }

        return null;
    }

    @Override
    protected String getActionName(PsiDirectory directory, String newName, String templateName) {
        return NEW_HASKELL_FILE;
    }
}
