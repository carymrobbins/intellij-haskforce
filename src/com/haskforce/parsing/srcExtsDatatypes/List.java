package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * List l [Exp l]
 */
public class List extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType[] exps;

    @Override
    public String toString() {
        return "List{" +
                Arrays.toString(exps) +
                '}';
    }
}
