package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  PrimChar   l Char     String
 */
public class PrimChar extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public char value;
    public String representation;

    @Override
    public String toString() {
        return "PrimChar{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
