package com.haskforce.eta.settings;

import com.haskforce.eta.jps.model.EtaBuildOptions;
import com.haskforce.eta.jps.model.JpsEtaBuildOptionsConstants;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

/** Persistent state of Eta build settings for a project. */
@State(
    name = JpsEtaBuildOptionsConstants.ETA_BUILD_OPTIONS_COMPONENT,
    storages = {
        @Storage(JpsEtaBuildOptionsConstants.ETA_BUILD_OPTIONS_FILE)
    }
)
public class EtaBuildSettings implements PersistentStateComponent<EtaBuildOptions> {

    public static EtaBuildSettings getInstance(Project project) {
        EtaBuildSettings settings = ServiceManager.getService(project, EtaBuildSettings.class);
        return settings == null ? new EtaBuildSettings() : settings;
    }

    private EtaBuildOptions state = new EtaBuildOptions();

    @Nullable
    @Override
    public EtaBuildOptions getState() {
        return state;
    }

    @Override
    public void loadState(EtaBuildOptions state) {
        this.state = state;
    }

    public void setEtaPath(String s) {
        state.etaPath = s;
    }

    public String getEtaPath() {
        return state.etaPath;
    }

    public void setEtaPkgPath(String s) {
        state.etaPkgPath = s;
    }

    public String getEtaPkgPath() {
        return state.etaPkgPath;
    }

    public void setEtlasPath(String s) {
        state.etlasPath = s;
    }

    public String getEtlasPath() {
        return state.etlasPath;
    }
}
