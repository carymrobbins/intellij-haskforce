package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  PrimString l String   String
 */
public class PrimString extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public String value;
    public String representation;

    @Override
    public String toString() {
        return "PrimString{" +
                "value='" + value + '\'' +
                ", representation='" + representation + '\'' +
                '}';
    }
}
