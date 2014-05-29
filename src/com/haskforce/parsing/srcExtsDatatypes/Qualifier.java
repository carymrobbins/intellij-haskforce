package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Qualifier l (Exp l)
 */
public class Qualifier extends StmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Qualifier{" +
                "exp=" + exp +
                '}';
    }
}
