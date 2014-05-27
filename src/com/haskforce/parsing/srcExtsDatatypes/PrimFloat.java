package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimFloat  l Rational String
 */
public class PrimFloat extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public float value;
    public String representation;

    @Override
    public String toString() {
        return "PrimFloat{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
