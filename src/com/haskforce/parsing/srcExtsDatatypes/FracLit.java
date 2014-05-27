package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Frac       l Rational String
 */
public class FracLit extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public float value;
    public String representation;

    @Override
    public String toString() {
        return "Frac{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
