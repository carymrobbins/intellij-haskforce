package com.haskforce;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleBuilderListener;
import com.intellij.ide.util.projectWizard.SourcePathsBuilder;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.roots.ModifiableRootModel;
import org.jetbrains.annotations.NotNull;

public class HaskellModuleBuilder extends JavaModuleBuilder implements SourcePathsBuilder, ModuleBuilderListener {
    @Override
    public void setupRootModel(ModifiableRootModel rootModel) throws ConfigurationException {
        addListener(this);
        super.setupRootModel(rootModel);
    }

    @Override
    public ModuleType getModuleType() {
        return HaskellModuleType.getInstance();
    }

    @Override
    public boolean isSuitableSdkType(SdkTypeId sdkType) {
        return sdkType == HaskellSdkType.getInstance();
    }

    @Override
    public void moduleCreated(@NotNull Module module) {
        // TODO
    }
}
