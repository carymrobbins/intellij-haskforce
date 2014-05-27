package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Int        l Integer  String
 */
public class IntLit extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public Integer value;
    public String representation;

    @Override
    public String toString() {
        return "IntLit{" +
                value +
                '}';
    }
}
