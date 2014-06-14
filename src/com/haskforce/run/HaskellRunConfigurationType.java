package com.haskforce.run;

import com.haskforce.HaskellIcons;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class HaskellRunConfigurationType extends ConfigurationTypeBase {
    public static final String HASKELL_RUN_CONFIG_ID = "Haskell Run Configuration";

    protected HaskellRunConfigurationType() {
        super(HASKELL_RUN_CONFIG_ID, "Haskell", "Run a Haskell application.", HaskellIcons.FILE);
        addFactory(new HaskellRunConfigurationFactory(this));
    }

    public static HaskellRunConfigurationType getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), HaskellRunConfigurationType.class);
    }
}
