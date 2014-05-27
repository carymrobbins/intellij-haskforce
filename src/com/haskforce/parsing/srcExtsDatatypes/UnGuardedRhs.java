package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnGuardedRhs l (Exp l)
 */
public class UnGuardedRhs extends RhsTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "UnGuardedRhs{" +
                "exp=" + exp +
                '}';
    }
}
