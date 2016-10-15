package com.haskforce.tools.cabal.run;

import com.haskforce.haskell.run.HaskellApplicationConfigurationFactory;
import com.haskforce.tools.cabal.CabalIcons;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;

/**
 * The "Cabal" option under Run->Edit Configurations->[+]
 */
public class CabalRunConfiguration extends ConfigurationTypeBase {
    public static final String HASKELL_CONFIG_ID = "Cabal Run Configuration";

    protected CabalRunConfiguration() {
        super(HASKELL_CONFIG_ID, "Cabal Run", "Execute a `cabal run` task.", CabalIcons.FILE);
        addFactory(new HaskellApplicationConfigurationFactory(this));
    }

    public static CabalRunConfiguration getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), CabalRunConfiguration.class);
    }
}
