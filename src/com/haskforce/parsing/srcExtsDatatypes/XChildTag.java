package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * XChildTag l [Exp l]
 */
public class XChildTag extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType[] exps;

    @Override
    public String toString() {
        return "XChildTag{" +
                "exps=" + Arrays.toString(exps) +
                '}';
    }
}
