package com.haskforce;

import com.intellij.ide.util.projectWizard.*;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.util.Condition;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

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

    @Override
    @Nullable
    public ModuleWizardStep modifySettingsStep(@NotNull SettingsStep settingsStep) {
        // Hook into the new project creation and set dist to the compiler output directory.
        final WizardContext c = settingsStep.getContext();
        if (c.isCreatingNewProject() && c.isProjectFileDirectorySet()) {
            c.setCompilerOutputDirectory(new File(c.getProjectFileDirectory(), "dist").getPath());
        }
        // Prompt the user to set the Haskell SDK for the project.
        return ProjectWizardStepFactory.getInstance().createJavaSettingsStep(settingsStep, this, new Condition<SdkTypeId>() {
            @Override
            public boolean value(SdkTypeId sdkType) {
                return isSuitableSdkType(sdkType);
            }
        });
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }
}
