package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Frac       l Rational String
 */
public class FracLit extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public float denominator;
    public float numerator;
    public String representation;

    @Override
    public String toString() {
        return "FracLit{" +
                "denominator=" + denominator +
                ", numerator=" + numerator +
                ", representation='" + representation + '\'' +
                '}';
    }
}
