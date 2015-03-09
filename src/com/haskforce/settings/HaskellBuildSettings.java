package com.haskforce.settings;

import com.haskforce.HaskellSdkType;
import com.haskforce.jps.model.HaskellBuildOptions;
import com.haskforce.jps.model.JpsHaskellBuildOptionsSerializer;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.SystemUtil;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.components.StorageScheme;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.List;

@State(
        name = JpsHaskellBuildOptionsSerializer.HASKELL_BUILD_OPTIONS_COMPONENT_NAME,
        storages = {
                @Storage(file = StoragePathMacros.PROJECT_FILE),
                @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/compiler.xml",
                        scheme = StorageScheme.DIRECTORY_BASED)
        }
)
public class HaskellBuildSettings implements PersistentStateComponent<HaskellBuildOptions> {
    private HaskellBuildOptions myBuildOptions = new HaskellBuildOptions();

    @Nullable
    @Override
    public HaskellBuildOptions getState() {
        return myBuildOptions;
    }

    @Override
    public void loadState(HaskellBuildOptions state) {
        myBuildOptions = state;
    }

    public boolean isCabalEnabled() {
        return myBuildOptions.myUseCabal;
    }

    public void setUseCabal(boolean useCabal) {
        myBuildOptions.myUseCabal = useCabal;
    }

    public boolean isCabalSandboxEnabled() {
        return myBuildOptions.myUseCabalSandbox;
    }

    public void setUseCabalSandbox(boolean useSandbox) {
        myBuildOptions.myUseCabalSandbox = useSandbox;
    }

    public boolean isInstallCabalDependenciesEnabled() {
        return myBuildOptions.myInstallCabalDependencies;
    }

    public boolean isEnableTestsEnabled() {
        return myBuildOptions.myEnableTests;
    }

    public void setInstallCabalDependencies(boolean install) {
        myBuildOptions.myInstallCabalDependencies = install;
    }

    public void setEnableTests(boolean enable) {
        myBuildOptions.myEnableTests = enable;
    }

    public boolean isProfilingEnabled() {
        return myBuildOptions.myProfilingBuild;
    }

    public void setProfilingBuild(boolean useDebugInfo) {
        myBuildOptions.myProfilingBuild = useDebugInfo;
    }

    @NotNull
    public String getGhcPath() {
        return myBuildOptions.myGhcPath;
    }

    public void setGhcPath(@NotNull String path) {
        myBuildOptions.myGhcPath = path;
    }

    @NotNull
    public String getCabalPath() {
        return myBuildOptions.myCabalPath;
    }

    public void setCabalPath(@NotNull String path) {
        myBuildOptions.myCabalPath = path;
    }

    @NotNull
    public String getCabalFlags() {
        return myBuildOptions.myCabalFlags;
    }

    public void setCabalFlags(@NotNull String flags) {
        myBuildOptions.myCabalFlags = flags;
    }

    @NotNull
    public List<String> getCabalFiles() {
        return myBuildOptions.myCabalFiles;
    }

    public void setCabalFiles(@NotNull List<String> files) {
        myBuildOptions.myCabalFiles = files;
    }

    public void updatePathsForProject(@NotNull Project project) {
        myBuildOptions.myGhcPath = guessGhcPathForProject(project);
        myBuildOptions.myCabalPath = guessCabalPathForProject(project);
    }

    private String guessGhcPathForProject(@NotNull Project project) {
        String path = myBuildOptions.myGhcPath;
        if (path == null || path.isEmpty() || path.equals(HaskellBuildOptions.DEFAULT_GHC_PATH)) {
            File sdkGhcPath = HaskellSdkType.getExecutable(project);
            if (sdkGhcPath != null && sdkGhcPath.canExecute()) return sdkGhcPath.getAbsolutePath();
        }
        return path;
    }

    private String guessCabalPathForProject(@NotNull Project project) {
        String path = myBuildOptions.myCabalPath;
        if (path == null || path.isEmpty() || path.equals(HaskellBuildOptions.DEFAULT_CABAL_PATH)) {
            File sdkGhcPath = HaskellSdkType.getExecutable(project);
            if (sdkGhcPath != null) {
                String parent = sdkGhcPath.getParent();
                File sdkCabalPath = new File(parent + SystemUtil.PATH_SEPARATOR + "cabal");
                if (sdkCabalPath.canExecute()) return sdkCabalPath.getAbsolutePath();
            }
            String foundCabalPath = ExecUtil.locateExecutable(HaskellBuildOptions.DEFAULT_CABAL_PATH);
            if (foundCabalPath != null && !foundCabalPath.isEmpty()) return foundCabalPath;
        }
        return path;
    }

    @NotNull
    public static HaskellBuildSettings getInstance(@NotNull Project project) {
        HaskellBuildSettings settings = ServiceManager.getService(project, HaskellBuildSettings.class);
        if (settings == null) settings = new HaskellBuildSettings();
        settings.updatePathsForProject(project);
        return settings;
    }
}
