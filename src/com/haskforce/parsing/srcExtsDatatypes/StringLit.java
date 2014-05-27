package com.haskforce.parsing.srcExtsDatatypes;

/**
 * String     l String   String
 */
public class StringLit extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public String value;
    public String representation;

    @Override
    public String toString() {
        return '"' + value + '"';
    }
}
