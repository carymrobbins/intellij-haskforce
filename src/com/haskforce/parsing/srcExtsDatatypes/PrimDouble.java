package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimDouble l Rational String
 */
public class PrimDouble extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public double denominator;
    public double numerator;
    public String representation;

    @Override
    public String toString() {
        return "PrimDouble{" +
                "denominator=" + denominator +
                ", numerator=" + numerator +
                ", representation='" + representation + '\'' +
                '}';
    }
}
