package com.haskforce.jps;

/*
 * Downloaded from https://github.com/ignatov/intellij-erlang on 7 May
 * 2014.
 */

import org.jetbrains.jps.builders.BuildRootDescriptor;
import org.jetbrains.jps.builders.BuildTarget;

import java.io.File;

/**
 *
 */
public class HaskellSourceRootDescriptor extends BuildRootDescriptor {
    private final File root;
    private final HaskellTarget target;

    public HaskellSourceRootDescriptor(File inRoot, HaskellTarget inTarget) {
        root = inRoot;
        target = inTarget;
    }

    @Override
    public String getRootId() {
        return "SourceRootDescriptor";
    }

    @Override
    public File getRootFile() {
        return root;
    }

    @Override
    public BuildTarget<?> getTarget() {
        return target;
    }
}
