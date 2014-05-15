package com.haskforce;

import com.haskforce.jps.model.JpsHaskellModelSerializerExtension;
import com.haskforce.utils.ExecUtil;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModel;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.util.SystemInfo;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

public class HaskellSdkType extends SdkType {
    public HaskellSdkType() {
        super(JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID);
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
        return JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID;
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
    public String getVersionString(final String sdkHome) {
        if (sdkHome == null) {
            return null;
        }
        File ghc = getExecutable(sdkHome);
        if (ghc.canExecute()) {
            return ExecUtil.run(ghc.getPath() + " --numeric-version");
        }
        return null;
    }

    @Nullable
    @Override
    public String suggestHomePath() {
        String libPath = suggestGhcLibDir();
        if (libPath == null) {
            return null;
        }
        return new File(libPath).getParentFile().getParent();
    }

    /**
     * Returns the value of ghc --print-libdir if ghc is available in PATH.
     */
    @Nullable
    public static String suggestGhcLibDir() {
        // Try running whatever GHC we have in $PATH.
        return ExecUtil.run("ghc --print-libdir");
    }

    /**
     * Best effort at locating GHC according to given path.
     */
    @NotNull
    public static File getExecutable(@NotNull final String path) {
        // We might get called with /usr/local/bin, or the true SDK
        // path. The goal is to run ghc at this stage, so adapt to whatever.
        String extra = path.endsWith("bin") ? "" : File.separator + "bin";
        return new File(path + extra, SystemInfo.isWindows ? "ghc.exe" : "ghc");
    }
}
