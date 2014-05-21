package com.haskforce;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleBuilderListener;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.SettingsStep;
import com.intellij.ide.util.projectWizard.SourcePathsBuilder;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.util.text.StringUtil;
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
        // TODO
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
}
