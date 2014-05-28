package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * data FunDep l = FunDep l [Name l] [Name l]
 */
public class FunDep {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType[] fromNames;
    public NameTopType[] toNames;

    @Override
    public String toString() {
        return "FunDep{" +
                "fromNames=" + Arrays.toString(fromNames) +
                ", toNames=" + Arrays.toString(toNames) +
                '}';
    }
}
