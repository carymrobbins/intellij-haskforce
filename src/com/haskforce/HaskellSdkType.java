package com.haskforce;

import com.haskforce.utils.ExecUtil;
import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.util.SystemInfo;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

public class HaskellSdkType extends SdkType {

    public static final String HASKELL_SDK_TYPE_ID = "Haskell SDK";

    public HaskellSdkType() {
        // TODO
        super(HASKELL_SDK_TYPE_ID);
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
        return HASKELL_SDK_TYPE_ID;
    }

    @Override
    public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {
    }

    @Override
    public boolean isValidSdkHome(String path) {
        File f = new File(path, SystemInfo.isWindows ? "ghc.exe" : "ghc");
        return f.canExecute();
    }

    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return new File(sdkHome).getName();
    }

    @Nullable
    @Override
    public String getVersionString(@NotNull final String sdkHome) {
        return new File(sdkHome).getName();
    }

    @Nullable
    @Override
    public String suggestHomePath() {
        // UNIX only, TODO: Windows
        return ExecUtil.exec("cat $(which ghc) | grep \"exedir=\\\".*\\\"\" | sed -E \"s/exedir=\\\"(.*)\\\"/\\1/\"");
    }
}
