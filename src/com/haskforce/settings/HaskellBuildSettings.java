package com.haskforce.settings;

import com.haskforce.jps.model.HaskellBuildOptions;
import com.haskforce.jps.model.JpsHaskellBuildOptionsSerializer;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

@State(
        name = JpsHaskellBuildOptionsSerializer.HASKELL_BUILD_OPTIONS_COMPONENT_NAME,
        storages = {
                @Storage("compiler.xml")
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
        if (useCabal) myBuildOptions.myUseStack = false;
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

    public boolean isStackEnabled() {
        return myBuildOptions.myUseStack;
    }

    public void setUseStack(boolean useStack) {
        myBuildOptions.myUseStack = useStack;
        if (useStack) myBuildOptions.myUseCabal = false;
    }

    public String getStackPath() {
        return myBuildOptions.myStackPath;
    }

    public void setStackPath(@NotNull String path) {
        myBuildOptions.myStackPath = path;
    }

    public String getStackFlags() {
        return myBuildOptions.myStackFlags;
    }

    public void setStackFlags(@NotNull String flags) {
        myBuildOptions.myStackFlags = flags;
    }

    public String getStackFile() {
        return myBuildOptions.myStackFile;
    }

    public void setStackFile(@NotNull String file) {
        myBuildOptions.myStackFile = file;
    }

    @NotNull
    public static HaskellBuildSettings getInstance(@NotNull Project project) {
        HaskellBuildSettings settings = ServiceManager.getService(project, HaskellBuildSettings.class);
        if (settings == null) settings = new HaskellBuildSettings();

        // TODO: Yeah this seems bad. Although we may be relying on this in places,
        // probably better to fix it up at the call sites.
        // settings.updatePaths();

        return settings;
    }

    @NotNull
    public static HaskellBuildSettings getDefault() {
        return getInstance(ProjectManager.getInstance().getDefaultProject());
    }
}
