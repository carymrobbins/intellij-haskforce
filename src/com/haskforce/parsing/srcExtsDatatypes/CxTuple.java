package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * CxTuple  l [Asst l]
 */
public class CxTuple extends ContextTopType {
    public SrcInfoSpan srcInfoSpan;
    public AsstTopType[] assts;

    @Override
    public String toString() {
        return "CxTuple{" +
                "assts=" + Arrays.toString(assts) +
                '}';
    }
}
