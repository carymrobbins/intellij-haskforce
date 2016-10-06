package com.haskforce.tools.stack.packages;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

//Java because we need public fields
@SuppressWarnings("WeakerAccess")
public class StackProjectsState implements Serializable {
    private static final long serialVersionUID = 1L;
    public final List<String> stackFiles;

    public StackProjectsState(List<String> stackFiles) {
        this.stackFiles = stackFiles;
    }

    @SuppressWarnings("unused")
    public StackProjectsState() {
        stackFiles = new ArrayList<>();
    }

}
