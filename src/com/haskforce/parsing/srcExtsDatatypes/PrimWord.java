package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimWord   l Integer  String
 */
public class PrimWord extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public Integer value;
    public String representation;

    @Override
    public String toString() {
        return "PrimWord{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
