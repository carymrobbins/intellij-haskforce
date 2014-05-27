package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ExpTypeSig l (Exp l) (Type l)
 */
public class ExpTypeSig extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public TypeTopType type;

    @Override
    public String toString() {
        return "ExpTypeSig{" +
                "exp=" + exp +
                ", type=" + type +
                '}';
    }
}
