package com.haskforce.run;

import com.haskforce.HaskellIcons;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;

/**
 * The "Haskel" option under Run->Edit Configurations->[+]
 */
public class HaskellConfigurationType extends ConfigurationTypeBase {
    public static final String HASKELL_CONFIG_ID = "Haskell Run Configuration";

    protected HaskellConfigurationType() {
        super(HASKELL_CONFIG_ID, "Haskell", "Run a Haskell application.", HaskellIcons.FILE);
        addFactory(new HaskellApplicationConfigurationFactory(this));
    }

    public static HaskellConfigurationType getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), HaskellConfigurationType.class);
    }
}
