package com.haskforce;

import com.haskforce.jps.model.JpsHaskellModelSerializerExtension;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModel;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

/**
 * Responsible for the mechanics when pressing "+" in the SDK configuration,
 * as well as the project SDK configuration.
 */
public class HaskellSdkType extends SdkType {
    // Messages go to the log available in Help -> Show log in finder.
    private final static Logger LOG = Logger.getInstance(HaskellSdkType.class);

    public HaskellSdkType() {
        super(JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID);
    }

    /**
     * Returns the Haskell SDK.
     */
    @NotNull
    public static HaskellSdkType getInstance() {
        return SdkType.findInstance(HaskellSdkType.class);
    }

    /**
     * Returns the icon to be used for Haskell things in general.
     */
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

    /**
     * Currently a no-op.
     */
    @Override
    public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {
    }

    /**
     * Approves of a path as SDK home.
     */
    @Override
    public boolean isValidSdkHome(String path) {
        // TODO: Validate SdkHome a bit more than just running ghc --version.
        if (path.endsWith("bin")) {
            LOG.info("The SDK home is the base directory of GHC.");
            LOG.info("Selected SDK home is: " + path);
            LOG.info("Aborting since there is no " + path + "/bin/ghc");
            return false;
        }
        String version = getVersionString(path);
        if (version == null) {
            // Error logging is performed by ExecUtil.
            return false;
        }
        if (!(version.startsWith("7.") || version.startsWith("8."))) {
            LOG.warn("Unexpected GHC version: " + version);
            LOG.warn("Accepting, but HaskForce might not work..");
        }
        return true;
    }

    /**
     * Always suggests "GHC" as the SDK name. We do not support anything else
     * in practice anyways.
     */
    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return "GHC";
    }

    /**
     * Returns the output of ghc --numeric-version for the given path.
     */
    @Nullable
    @Override
    public String getVersionString(final String sdkHome) {
        if (sdkHome == null) {
            return null;
        }
        File ghc = getExecutable(sdkHome);
        if (ghc.canExecute()) {
            return ExecUtil.readCommandLine(new GeneralCommandLine(ghc.getPath(), "--numeric-version"));
        }
        return null;
    }

    /**
     * Suggests a home path two levels up from GHC's reported libdir.
     */
    @Nullable
    @Override
    public String suggestHomePath() {
        String libPath = suggestGhcLibDir();
        if (libPath == null) {
            LOG.warn("Could not run ghc --print-libdir.");
            LOG.warn("Please ensure that ghc is in your PATH.");
            return null;
        } else if (libPath.isEmpty()) {
            LOG.warn("Did not get any output from ghc --print-libdir.");
            return null;
        }
        final File libDir = new File(libPath);
        // On some platforms (e.g. Windows), libDir will point to the actual lib directory instead of lib/ghc-x.x.x
        return libDir.getName().equals("lib") ? libDir.getParent() : libDir.getParentFile().getParent();
    }

    /**
     * Produces the grey "7.6.3" version number next to GHC in the project
     * settings.
     */
    @Override
    public boolean setupSdkPaths(Sdk sdk, SdkModel sdkModel) {
        super.setupSdkPaths(sdk, sdkModel);
        final SdkModificator sdkModificator = sdk.getSdkModificator();
        final String homePath = sdk.getHomePath();
        sdkModificator.setVersionString(getVersionString(homePath));
        sdkModificator.commitChanges();
        return true;
    }

    /**
     * Returns the value of ghc --print-libdir if ghc is available in PATH.
     */
    @Nullable
    public static String suggestGhcLibDir() {
        // Try running whatever GHC we have in $PATH.
        return ExecUtil.exec("ghc --print-libdir");
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

    @Nullable
    public static File getExecutable(@NotNull final Project project) {
        final String sdkPath = getHaskellSdkPath(project);
        return sdkPath == null ? null : getExecutable(sdkPath);
    }

    /**
     * Gets the Haskell SDK path for a project.
     */
    public static String getHaskellSdkPath(@NotNull Project project) {
        Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
        // This sdk is not an instanceof HaskellSdkType. Compare with name instead.
        if (sdk == null || !sdk.getSdkType().toString().equals(JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID)) {
            return null;
        }
        return sdk.getHomePath();
    }
}
