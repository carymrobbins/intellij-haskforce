package com.haskforce.settings;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * String wrapper to ensure that we don't accidentally pass an invalid key to the PropertiesComponent.
 * These are set as property keys in HaskellToolsConfigurable.
 */
public class ToolKey {
    public static final ToolKey STYLISH_HASKELL_KEY = new ToolKey("stylishHaskell", "stylish-haskell");
    public static final ToolKey HLINT_KEY = new ToolKey("hlint", "hlint");
    public static final ToolKey GHC_MOD_KEY = new ToolKey("ghcMod", "ghc-mod");
    public static final ToolKey GHC_MODI_KEY = new ToolKey("ghcModi", "ghc-modi");

    public static final String GHC_MODI_TIMEOUT_KEY = "ghcModiTimeout";
    public static final long GHC_MODI_TIMEOUT_DEFAULT = 5000;

    public static long getGhcModiTimeout(@NotNull Project project) {
        final String timeout = PropertiesComponent.getInstance(project).getValue(GHC_MODI_TIMEOUT_KEY);
        if (timeout == null || timeout.isEmpty()) return GHC_MODI_TIMEOUT_DEFAULT;
        try {
            return Long.parseLong(timeout);
        } catch (NumberFormatException e) {
            return GHC_MODI_TIMEOUT_DEFAULT;
        }
    }

    public final String prettyName;
    public final String pathKey;
    public final String flagsKey;

    public ToolKey(String name, String prettyName) {
        this.prettyName = prettyName;
        this.pathKey = name + "Path";
        this.flagsKey = name + "Flags";
    }

    @Nullable
    public String getPath(@NotNull Project project) {
        final String path = PropertiesComponent.getInstance(project).getValue(pathKey);
        return path == null || path.isEmpty() ? null : path;
    }

    @NotNull
    public String getFlags(@NotNull Project project) {
        final String flags = PropertiesComponent.getInstance(project).getValue(flagsKey);
        return flags == null ? "" : flags;
    }

    @Override
    public String toString() {
        return "ToolKey(" + prettyName + ")";
    }
}
