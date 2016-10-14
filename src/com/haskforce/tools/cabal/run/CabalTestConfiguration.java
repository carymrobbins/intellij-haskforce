package com.haskforce.tools.cabal.run;

import com.haskforce.haskell.run.HaskellTestConfigurationFactory;
import com.haskforce.tools.cabal.CabalIcons;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.util.containers.ContainerUtil;

/**
 * The "Cabal" option under Run->Edit Configurations->[+]
 */
public class CabalTestConfiguration extends ConfigurationTypeBase {
    public static final String HASKELL_CONFIG_ID = "Cabal Test Configuration";

    protected CabalTestConfiguration() {
        super(HASKELL_CONFIG_ID, "Cabal Test", "Execute a `cabal test` task.", CabalIcons.FILE);
        addFactory(new HaskellTestConfigurationFactory(this));
    }

    public static CabalTestConfiguration getInstance() {
        return ContainerUtil.findInstance(Extensions.getExtensions(CONFIGURATION_TYPE_EP), CabalTestConfiguration.class);
    }
}
