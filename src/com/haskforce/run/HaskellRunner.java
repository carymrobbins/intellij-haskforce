package com.haskforce.run;

import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.runners.DefaultProgramRunner;
import org.jetbrains.annotations.NotNull;

public class HaskellRunner extends DefaultProgramRunner {
    public static final String HASKELL_RUNNER_ID = "HaskellRunner";

    @NotNull
    public String getRunnerId() {
        return HASKELL_RUNNER_ID;
    }

    public boolean canRun(@NotNull final String executorId, @NotNull final RunProfile profile) {
        return executorId.equals(DefaultRunExecutor.EXECUTOR_ID) && profile instanceof HaskellRunConfigurationBase;
    }
}
