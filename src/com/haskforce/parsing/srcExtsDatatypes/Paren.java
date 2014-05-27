package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Paren l (Exp l)
 */
public class Paren extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "(" + exp + ")";
    }
}
