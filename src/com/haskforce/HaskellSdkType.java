package com.haskforce;

import com.intellij.openapi.projectRoots.*;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HaskellSdkType extends SdkType {
    public HaskellSdkType() {
        // TODO
        super("Haskell SDK");
    }

    @NotNull
    public static HaskellSdkType getInstance() {
        return SdkType.findInstance(HaskellSdkType.class);
    }

    @Override
    public Icon getIcon() {
        return HaskellIcons.FILE;
    }

    @Nullable
    @Override
    public AdditionalDataConfigurable createAdditionalDataConfigurable(SdkModel sdkModel,
                                                                       SdkModificator sdkModificator) {
        return null;
    }

    @Override
    public String getPresentableName() {
        return null;
    }

    @Override
    public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {
    }

    @Override
    public boolean isValidSdkHome(String path) {
        // TODO
        return false;
    }

    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return null;
    }

    @Nullable
    @Override
    public String suggestHomePath() {
        // TODO
        return null;
    }
}
