package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  CorePragma l      String (Exp l)
 */
public class CorePragma extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "CorePragma{" +
                "s='" + s + '\'' +
                ", exp=" + exp +
                '}';
    }
}
