package com.haskforce;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleBuilderListener;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.SettingsStep;
import com.intellij.ide.util.projectWizard.SourcePathsBuilder;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.util.text.StringUtil;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HaskellModuleBuilder extends JavaModuleBuilder implements SourcePathsBuilder, ModuleBuilderListener {
    @Override
    public void setupRootModel(ModifiableRootModel rootModel) throws ConfigurationException {
        addListener(this);
        super.setupRootModel(rootModel);
    }

    /**
     * Returns the Haskell module type.
     */
    @Override
    public ModuleType getModuleType() {
        return HaskellModuleType.getInstance();
    }

    /**
     * Ensures that SDK type is a Haskell SDK.
     */
    @Override
    public boolean isSuitableSdkType(SdkTypeId sdkType) {
        return sdkType == HaskellSdkType.getInstance();
    }

    /**
     * Called after module is created.
     */
    @Override
    public void moduleCreated(@NotNull Module module) {
        // TODO - We should probably do some project initialization here as well...

        // Update the ignored files and folders to avoid file search showing compiled files.
        FileTypeManager fileTypeManager = FileTypeManager.getInstance();
        StringBuilder builder = new StringBuilder(fileTypeManager.getIgnoredFilesList());
        // Ensure the ignored file list ends with a semicolon.
        if (builder.charAt(builder.length() - 1) != ';') {
            builder.append(';');
        }
        for (String ignore : new String[]{"*.dyn_hi", "*.dyn_hi", "*.dyn_o", "*.hi", "*.o"}) {
            if (builder.indexOf(';' + ignore + ';') == -1) {
                builder.append(ignore).append(';');
            }
        }
        fileTypeManager.setIgnoredFilesList(builder.toString());
    }

    /**
     * Hook into the new project creation and set dist to the compiler output
     * directory.
     */
    @Override
    @Nullable
    public ModuleWizardStep modifySettingsStep(@NotNull SettingsStep settingsStep) {
        if (settingsStep.getContext().isCreatingNewProject() &&
                settingsStep.getContext().isProjectFileDirectorySet()) {
            WizardContext c = settingsStep.getContext();
            String path = c.getProjectFileDirectory();
            String out = StringUtil.endsWithChar(path, '/') ? path + "dist" : path + "/dist";
            c.setCompilerOutputDirectory(out);
        }
        return super.modifySettingsStep(settingsStep);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }
}
