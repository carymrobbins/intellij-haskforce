package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * GuardedRhss  l [GuardedRhs l]
 */
public class GuardedRhss extends RhsTopType {
    public SrcInfoSpan srcInfoSpan;
    public GuardedRhs[] rhsses;

    @Override
    public String toString() {
        return "GuardedRhss{" +
                "rhsses=" + Arrays.toString(rhsses) +
                '}';
    }
}
