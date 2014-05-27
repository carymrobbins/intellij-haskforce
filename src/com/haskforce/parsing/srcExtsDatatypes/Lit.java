package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Lit l (Literal l)
 */
public class Lit extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public LiteralTopType literal;

    @Override
    public String toString() {
        return "Lit{" +
                "literal=" + literal +
                '}';
    }
}
