package com.haskforce.highlighting.annotation;


import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Arrays;

public class Problems extends ArrayList<HaskellProblem> {
    public Problems() {
        super();
    }

    public Problems(int initialCapacity) {
        super(initialCapacity);
    }

    public Problems(@NotNull HaskellProblem[] problems) {
        super(Arrays.asList(problems));
    }

    public void addAllNotNull(Iterable<? extends HaskellProblem> elements) {
        if (elements != null) {
            ContainerUtil.addAllNotNull(this, elements);
        }
    }
}
