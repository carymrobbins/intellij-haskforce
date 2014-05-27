package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnGuardedAlt l (Exp l)
 */
public class UnGuardedAlt extends GuardedAltsTopType { // TODO: Deserialize?
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e;

    @Override
    public String toString() {
        return "UnGuardedAlt{" +
                e +
                '}';
    }
}
