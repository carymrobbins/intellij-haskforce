package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PromotedList l Bool [Promoted l]
 */
public class PromotedList extends PromotedTopType {
    public SrcInfoSpan srcInfoSpan;
    public boolean leadingQuote;
    public PromotedTopType[] promoteds;

    @Override
    public String toString() {
        return "PromotedList{" +
                "leadingQuote=" + leadingQuote +
                ", promoteds=" + Arrays.toString(promoteds) +
                '}';
    }
}
