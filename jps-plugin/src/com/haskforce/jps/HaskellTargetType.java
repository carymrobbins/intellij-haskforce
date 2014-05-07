package com.haskforce.jps;

/*
 * Downloaded from https://github.com/ignatov/intellij-erlang on 7 May
 * 2014.
 */

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildTargetLoader;
import org.jetbrains.jps.builders.ModuleBasedBuildTargetType;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.module.JpsTypedModule;

import java.util.ArrayList;
import java.util.List;

/**
 * Target type helpers.
 */
public class HaskellTargetType extends ModuleBasedBuildTargetType<HaskellTarget> {
    public static final HaskellTargetType PRODUCTION = new HaskellTargetType("haskell-production", false);
    public static final HaskellTargetType TESTS = new HaskellTargetType("haskell-tests", true);
    private final boolean test;

    public HaskellTargetType(String name, boolean inTest) {
        super(name);
        test = inTest;
    }

    @NotNull
    @Override
    public List<HaskellTarget> computeAllTargets(@NotNull JpsModel jpsModel) {
        List<HaskellTarget> targets = new ArrayList<HaskellTarget>();
        for (JpsTypedModule<JpsDummyElement> module : jpsModel.getProject().getModules(JpsHaskellModuleType.INSTANCE)) {
            targets.add(new HaskellTarget(module, this));
        }
        return targets;
    }

    @NotNull
    @Override
    public BuildTargetLoader<HaskellTarget> createLoader(@NotNull final JpsModel jpsModel) {
        return new BuildTargetLoader<HaskellTarget>() {
            @Nullable
            @Override
            public HaskellTarget createTarget(@NotNull String targetId) {
                for (JpsTypedModule<JpsDummyElement> module : jpsModel.getProject().getModules(JpsHaskellModuleType.INSTANCE)) {
                    if (module.getName().equals(targetId)) {
                        return new HaskellTarget(module, HaskellTargetType.this);
                    }
                }
                return null;
            }
        };
    }

    public boolean isTests() {
        return test;
    }
}
