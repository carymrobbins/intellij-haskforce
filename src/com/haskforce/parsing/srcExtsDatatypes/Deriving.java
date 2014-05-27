package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *
 * data Deriving l = Deriving l [InstHead l]
 */
public class Deriving {
    public SrcInfoSpan srcInfoSpan;
    public InstHeadTopType[] instHeads;

    @Override
    public String toString() {
        return "Deriving{" +
                "instHeads=" + Arrays.toString(instHeads) +
                '}';
    }
}
