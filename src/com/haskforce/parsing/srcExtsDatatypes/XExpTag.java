package com.haskforce.parsing.srcExtsDatatypes;

/**
 * XExpTag l (Exp l)
 */
public class XExpTag extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "XExpTag{" +
                "exp=" + exp +
                '}';
    }
}
