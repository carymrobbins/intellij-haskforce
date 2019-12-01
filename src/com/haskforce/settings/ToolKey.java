package com.haskforce.settings;

import com.haskforce.utils.NotificationUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * String wrapper to ensure that we don't accidentally pass an invalid key to the PropertiesComponent.
 * These are set as property keys in HaskellToolsConfigurable.
 */
public class ToolKey {

    private static Logger LOG = Logger.getInstance(ToolKey.class);

    public static final ToolKey STYLISH_HASKELL_KEY = new ToolKey("stylishHaskell", "stylish-haskell");
    public static final ToolKey HLINT_KEY = new ToolKey("hlint", "hlint");
    public static final ToolKey GHC_MOD_KEY = new ToolKey("ghcMod", "ghc-mod");
    public static final ToolKey GHC_MODI_KEY = new ToolKey("ghcModi", "ghc-modi");
    public static final ToolKey HSDEV_KEY = new ToolKey("hsdev", "hsdev");
    public static final ToolKey HINDENT_KEY = new ToolKey("hindent", "hindent");

    public static final String GHC_MODI_RESPONSE_TIMEOUT_LEGACY_KEY = "ghcModiTimeout";
    public static final String GHC_MODI_RESPONSE_TIMEOUT_KEY = "ghcModiTimeout";
    public static final long GHC_MODI_RESPONSE_TIMEOUT_DEFAULT = 5000; // 5 seconds

    public static final String GHC_MODI_KILL_IDLE_TIMEOUT_KEY = "ghcModiKillIdleTimeout";
    public static final long GHC_MODI_KILL_IDLE_TIMEOUT_DEFAULT = 600000; // 10 minutes

    private static String getGhcModiResponseTimeoutString(@NotNull Project project) {
        String timeout;
        for (String k : new String[]{GHC_MODI_RESPONSE_TIMEOUT_KEY, GHC_MODI_RESPONSE_TIMEOUT_LEGACY_KEY}) {
            timeout = PropertiesComponent.getInstance(project).getValue(k);
            if (timeout != null && !timeout.isEmpty()) return timeout;
        }
        return null;
    }

    public static long getGhcModiResponseTimeout(@NotNull Project project) {
        String timeout = getGhcModiResponseTimeoutString(project);
        if (timeout == null) return GHC_MODI_RESPONSE_TIMEOUT_DEFAULT;
        try {
            return Long.parseLong(timeout);
        } catch (NumberFormatException e) {
            String message = "Invalid " + GHC_MODI_RESPONSE_TIMEOUT_KEY + " value '" + timeout + "'"
                           + "; defaulting to " + GHC_MODI_RESPONSE_TIMEOUT_DEFAULT;
            LOG.warn(message);
            NotificationUtil.displaySimpleNotification(NotificationType.WARNING, project, "Configuration", message);
            return GHC_MODI_RESPONSE_TIMEOUT_DEFAULT;
        }
    }

    public static long getGhcModiKillIdleTimeout(@NotNull Project project) {
        final String timeout = PropertiesComponent.getInstance(project).getValue(GHC_MODI_KILL_IDLE_TIMEOUT_KEY);
        if (timeout == null || timeout.isEmpty()) return GHC_MODI_KILL_IDLE_TIMEOUT_DEFAULT;
        try {
            return Long.parseLong(timeout);
        } catch (NumberFormatException e) {
            String message = "Invalid " + GHC_MODI_KILL_IDLE_TIMEOUT_KEY + " value '" + timeout + "'"
                           + "; default to " + GHC_MODI_KILL_IDLE_TIMEOUT_DEFAULT;
            LOG.warn(message);
            NotificationUtil.displaySimpleNotification(NotificationType.WARNING, project, "Configuration", message);
            return GHC_MODI_KILL_IDLE_TIMEOUT_DEFAULT;
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
