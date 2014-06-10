package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PrimFloat  l Rational String
 */
public class PrimFloat extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public float denominator;
    public float numerator;
    public String representation;

    @Override
    public String toString() {
        return "PrimFloat{" +
                "denominator=" + denominator +
                ", numerator=" + numerator +
                ", representation='" + representation + '\'' +
                '}';
    }
}
