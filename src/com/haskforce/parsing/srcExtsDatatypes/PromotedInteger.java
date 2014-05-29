package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PromotedInteger l Integer String
 */
public class PromotedInteger extends PromotedTopType {
    public SrcInfoSpan srcInfoSpan;
    public Integer value;
    public String representation;

    @Override
    public String toString() {
        return "PromotedInteger{" +
                "value=" + value +
                '}';
    }
}
