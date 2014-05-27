package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimDouble l Rational String
 */
public class PrimDouble extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public double value;
    public String representation;

    @Override
    public String toString() {
        return "PrimDouble{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
