package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PromotedTuple l [Promoted l]
 */
public class PromotedTuple extends PromotedTopType {
    public SrcInfoSpan srcInfoSpan;
    public PromotedTopType[] promoteds;

    @Override
    public String toString() {
        return "PromotedTuple{" +
                "promoteds=" + Arrays.toString(promoteds) +
                '}';
    }
}
