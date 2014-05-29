package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PromotedString l String String
 */
public class PromotedString extends PromotedTopType {
    public SrcInfoSpan srcInfoSpan;
    public String value;
    public String representation;

    @Override
    public String toString() {
        return "PromotedString{" +
                "value='" + value + '\'' +
                '}';
    }
}
