package com.haskforce.settings;

import com.haskforce.jps.model.HaskellBuildOptions;
import com.haskforce.jps.model.JpsHaskellBuildOptionsSerializer;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.components.StoragePathMacros;
import com.intellij.openapi.components.StorageScheme;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
    public List<String> getCabalFiles() {
        return myBuildOptions.myCabalFiles;
    }

    public void setCabalFiles(@NotNull List<String> files) {
        myBuildOptions.myCabalFiles = files;
    }

    @NotNull
    public static HaskellBuildSettings getInstance(@NotNull Project project) {
        final HaskellBuildSettings persisted = ServiceManager.getService(project, HaskellBuildSettings.class);
        return persisted != null ? persisted : new HaskellBuildSettings();
    }
}
