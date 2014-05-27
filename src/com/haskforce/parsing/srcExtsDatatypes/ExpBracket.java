package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ExpBracket l (Exp l)
 */
public class ExpBracket extends BracketTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "ExpBracket{" +
                "exp=" + exp +
                '}';
    }
}
