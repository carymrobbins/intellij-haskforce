package com.haskforce.tools.stack.packages;

import com.intellij.util.xmlb.annotations.Attribute;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class StackProjectsState implements Serializable {
    private static final long serialVersionUID = 1L;
    @Attribute
    public final String stackFiles;

    public StackProjectsState(List<String> stackFiles) {
        this.stackFiles = stackFiles.stream().collect(Collectors.joining(";"));
    }

    @SuppressWarnings("unused")
    public StackProjectsState() {
        stackFiles = "";
    }

    public List<String> getFiles() {
        return Arrays.asList(stackFiles.split(";"));
    }
}
