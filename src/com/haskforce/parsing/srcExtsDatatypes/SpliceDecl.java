package com.haskforce.parsing.srcExtsDatatypes;

/**
 * SpliceDecl   l (Exp l)
 */
public class SpliceDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "SpliceDecl{" +
                "exp=" + exp +
                '}';
    }
}
