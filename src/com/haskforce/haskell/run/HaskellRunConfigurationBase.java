package com.haskforce.haskell.run;

import com.haskforce.settings.HaskellBuildSettings;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.NotificationUtil;
import com.haskforce.utils.EitherUtil;
import com.intellij.execution.configuration.AbstractRunConfiguration;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RuntimeConfigurationError;
import com.intellij.execution.configurations.RuntimeConfigurationException;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import scala.util.Either;

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
        GeneralCommandLine cabalCmdLine = new GeneralCommandLine(cabalPath, "--numeric-version");
        Either<ExecUtil.ExecError, String> result = ExecUtil.readCommandLine(cabalCmdLine);
        if (result.isLeft()) {
            //noinspection ThrowableResultOfMethodCallIgnored
            ExecUtil.ExecError e = EitherUtil.unsafeGetLeft(result);
            NotificationUtil.displaySimpleNotification(
                NotificationType.ERROR, getProject(), "cabal", e.getMessage()
            );
            throw new RuntimeConfigurationError("Failed executing cabal to check its version: " + e.getMessage());
        }
        final String out = EitherUtil.unsafeGetRight(result);
        final Matcher m = EXTRACT_CABAL_VERSION_REGEX.matcher(out);
        if (!m.find()) {
            throw new RuntimeConfigurationError("Could not parse cabal version: '" + out + "'");
        }
        final Double actualVersion = Double.parseDouble(m.group(1));
        if (actualVersion < minimumVersion) {
            throw new RuntimeConfigurationError(errorMessage);
        }
    }
}
