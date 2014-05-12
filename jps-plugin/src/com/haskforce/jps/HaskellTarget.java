package com.haskforce.jps;

/*
 * Based on the ErlangTarget at https://github.com/ignatov/intellij-erlang on
 * 7 May 2014.
 */


import com.haskforce.jps.model.JpsHaskellModuleType;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildRootIndex;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.BuildTargetRegistry;
import org.jetbrains.jps.builders.ModuleBasedTarget;
import org.jetbrains.jps.builders.TargetOutputIndex;
import org.jetbrains.jps.builders.storage.BuildDataPaths;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.indices.IgnoredFileIndex;
import org.jetbrains.jps.indices.ModuleExcludeIndex;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.java.JavaSourceRootProperties;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.java.JpsJavaClasspathKind;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsTypedModuleSourceRoot;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * Various helper functions relating to targets.
 */
public class HaskellTarget extends ModuleBasedTarget<HaskellSourceRootDescriptor> {
    public HaskellTarget(@NotNull JpsModule module, HaskellTargetType targetType) {
        super(targetType, module);
    }

    @Override
    public String getId() {
        return myModule.getName();
    }

    @Override
    public Collection<BuildTarget<?>> computeDependencies(BuildTargetRegistry buildTargetRegistry, TargetOutputIndex targetOutputIndex) {
        List<BuildTarget<?>> dependencies = new ArrayList<BuildTarget<?>>();
        Set<JpsModule> modules = JpsJavaExtensionService.dependencies(myModule).includedIn(JpsJavaClasspathKind.compile(isTests())).getModules();
        for (JpsModule module : modules) {
            if (module.getModuleType().equals(JpsHaskellModuleType.INSTANCE)) {
                dependencies.add(new HaskellTarget(module, getHaskellTargetType()));
            }
        }
        if (isTests()) {
            dependencies.add(new HaskellTarget(myModule, HaskellTargetType.PRODUCTION));
        }
        return dependencies;    }

    @NotNull
    @Override
    public List<HaskellSourceRootDescriptor> computeRootDescriptors(JpsModel jpsModel, ModuleExcludeIndex moduleExcludeIndex, IgnoredFileIndex ignoredFileIndex, BuildDataPaths buildDataPaths) {
        List<HaskellSourceRootDescriptor> result = new ArrayList<HaskellSourceRootDescriptor>();
        JavaSourceRootType type = isTests() ? JavaSourceRootType.TEST_SOURCE : JavaSourceRootType.SOURCE;
        for (JpsTypedModuleSourceRoot<JavaSourceRootProperties> root : myModule.getSourceRoots(type)) {
            result.add(new HaskellSourceRootDescriptor(root.getFile(), this));
        }
        return result;
    }


    @Nullable
    @Override
    public HaskellSourceRootDescriptor findRootDescriptor(String s, BuildRootIndex buildRootIndex) {
        return ContainerUtil.getFirstItem(buildRootIndex.getRootDescriptors(new File(s), Collections.singletonList(getHaskellTargetType()), null));
    }

    @NotNull
    @Override
    public String getPresentableName() {
        return getId(); // TODO: Update to a proper name.
    }

    @NotNull
    @Override
    public Collection<File> getOutputRoots(CompileContext compileContext) {
        return ContainerUtil.createMaybeSingletonList(JpsJavaExtensionService.getInstance().getOutputDirectory(myModule, isTests()));
    }

    public HaskellTargetType getHaskellTargetType() {
        return (HaskellTargetType) getTargetType();
    }

    @Override
    public boolean isTests() {
        return getHaskellTargetType().isTests();
    }
}
