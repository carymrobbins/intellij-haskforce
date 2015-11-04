package com.haskforce.run;

import com.haskforce.HaskellIcons;
import com.haskforce.cabal.CabalIcons;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;

/**
 * The "Haskel" option under Run->Edit Configurations->[+]
 */
public class HaskellApplicationConfigurationType extends ConfigurationTypeBase {
    public static final String HASKELL_CONFIG_ID = "Cabal Run Configuration";

    protected HaskellApplicationConfigurationType() {
        super(HASKELL_CONFIG_ID, "Cabal Run", "Execute a `cabal run` task.", CabalIcons.FILE);
        addFactory(new HaskellApplicationConfigurationFactory(this));
    }

    public static HaskellApplicationConfigurationType getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), HaskellApplicationConfigurationType.class);
    }
}
