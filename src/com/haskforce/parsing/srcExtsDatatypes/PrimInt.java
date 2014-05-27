package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimInt    l Integer  String
 */
public class PrimInt extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public Integer value;
    public String representation;

    @Override
    public String toString() {
        return "PrimInt{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
