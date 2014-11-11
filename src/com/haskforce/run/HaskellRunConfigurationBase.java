package com.haskforce.run;

import com.haskforce.settings.HaskellBuildSettings;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.configuration.AbstractRunConfiguration;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RuntimeConfigurationError;
import com.intellij.execution.configurations.RuntimeConfigurationException;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class HaskellRunConfigurationBase extends AbstractRunConfiguration {
    public HaskellRunConfigurationBase(Project project, ConfigurationFactory factory) {
        super(project, factory);
    }

    public static final Pattern EXTRACT_CABAL_VERSION_REGEX = Pattern.compile("(\\d+\\.\\d+).*");

    protected void requireCabal1_18() throws RuntimeConfigurationException {
        requireCabalVersionMinimum(1.18, "Run configurations require cabal 1.18 or higher.");
    }

    protected void requireCabalVersionMinimum(double minimumVersion, @NotNull String errorMessage) throws RuntimeConfigurationException {
        final HaskellBuildSettings buildSettings = HaskellBuildSettings.getInstance(getProject());
        final String cabalPath = buildSettings.getCabalPath();
        if (cabalPath.isEmpty()) {
            throw new RuntimeConfigurationError("Path to cabal is not set.");
        }
        final String out = ExecUtil.readCommandLine(null, cabalPath, "--numeric-version");
        if (out == null) {
            throw new RuntimeConfigurationError("Failed executing cabal to check its version.");
        }
        final Matcher m = EXTRACT_CABAL_VERSION_REGEX.matcher(out);
        if (!m.find()) {
            throw new RuntimeConfigurationError("Could not parse cabal version.");
        }
        final Double actualVersion = Double.parseDouble(m.group(1));
        if (actualVersion < minimumVersion) {
            throw new RuntimeConfigurationError(errorMessage);
        }
    }
}
