package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *ImportSpecList l Bool [ImportSpec l]
 */
public class ImportSpecList {
    public SrcInfoSpan srcInfoSpan;
    public boolean hiding;
    public ImportSpecTopType[] importSpecs;

    @Override
    public String toString() {
        return "ImportSpecList{" +
                (hiding ? "hiding=" : "") +
                ", importSpecs=" + Arrays.toString(importSpecs) +
                '}';
    }
}
