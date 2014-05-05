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
        return getVersionString(path) != null;
    }

    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return "GHC";
    }

    @Nullable
    @Override
    public String getVersionString(@NotNull final String sdkHome) {
        File ghc = getExecutable(sdkHome);
        if (ghc.canExecute()) {
            return ExecUtil.exec(String.format("\"%s\" --numeric-version", ghc.getPath()));
        }
        return null;
    }

    @Nullable
    @Override
    public String suggestHomePath() {
        return SystemInfo.isWindows ? suggestHomePathForWindows() : suggestHomePathForUNIX();
    }

    @Nullable
    public static String suggestHomePathForWindows() {
        final String ghcPath = ExecUtil.exec("where ghc.exe");
        if (ghcPath != null) {
            File ghc = new File(ghcPath);
            if (ghc.canExecute()) {
                return ghc.getParent();
            }
        }
        return null;
    }

    @Nullable
    public static String suggestHomePathForUNIX() {
        return ExecUtil.run("ghc --print-libdir");
    }

    @NotNull
    public static File getExecutable(@NotNull final String path) {
        return new File(path, SystemInfo.isWindows ? "ghc.exe" : "ghc");
    }
}
