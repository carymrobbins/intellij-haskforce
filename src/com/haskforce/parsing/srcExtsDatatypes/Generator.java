package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Generator l (Pat l) (Exp l)
 */
public class Generator extends StmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Generator{" +
                "pat=" + pat +
                ", exp=" + exp +
                '}';
    }
}
