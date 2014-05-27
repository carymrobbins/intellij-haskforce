package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EnumFrom l (Exp l)
 */
public class EnumFrom extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "EnumFrom{" +
                "exp=" + exp +
                '}';
    }
}
