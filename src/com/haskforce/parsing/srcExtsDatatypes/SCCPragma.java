package com.haskforce.parsing.srcExtsDatatypes;

/**
 * SCCPragma  l      String (Exp l)
 */
public class SCCPragma extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "SCCPragma{" +
                "s='" + s + '\'' +
                ", exp=" + exp +
                '}';
    }
}
