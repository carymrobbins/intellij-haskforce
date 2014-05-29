package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * KindList  l [Kind l]
 */
public class KindList extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public KindTopType[] kinds;

    @Override
    public String toString() {
        return "KindList{" +
                "kinds=" + Arrays.toString(kinds) +
                '}';
    }
}
