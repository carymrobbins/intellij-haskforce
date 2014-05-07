package com.haskforce.jps;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.BuilderService;
import org.jetbrains.jps.incremental.ModuleLevelBuilder;
import org.jetbrains.jps.incremental.TargetBuilder;

import java.util.Arrays;
import java.util.List;

/**
 * Main entry for the external builder interface.
 */
public class HaskForceBuilder extends BuilderService {
    @NotNull
    @Override
    public List<? extends ModuleLevelBuilder> createModuleLevelBuilders() {
        return Arrays.asList(new CabalBuilder());
    }

    @NotNull
    @Override
    public List<HaskellTargetType> getTargetTypes() {
        return Arrays.asList(HaskellTargetType.PRODUCTION, HaskellTargetType.TESTS);
    }
}
