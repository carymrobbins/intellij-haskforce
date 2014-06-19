package com.haskforce.run;

import com.haskforce.HaskellIcons;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;

/**
 * The "Haskel" option under Run->Edit Configurations->[+]
 */
public class HaskellTestConfigurationType extends ConfigurationTypeBase {
    public static final String HASKELL_CONFIG_ID = "Haskell Test Configuration";

    protected HaskellTestConfigurationType() {
        super(HASKELL_CONFIG_ID, "Haskell Test", "Run Haskell tests.", HaskellIcons.TEST_FILE);
        addFactory(new HaskellTestConfigurationFactory(this));
    }

    public static HaskellTestConfigurationType getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), HaskellTestConfigurationType.class);
    }
}
