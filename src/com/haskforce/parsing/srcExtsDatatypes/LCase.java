package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * LCase l [Alt l]
 */
public class LCase extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public Alt[] alts;

    @Override
    public String toString() {
        return "LCase{" +
                "alts=" + Arrays.toString(alts) +
                '}';
    }
}
