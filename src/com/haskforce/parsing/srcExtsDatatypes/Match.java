package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Match l      (Name l) [Pat l]         (Rhs l) {-where-} (Maybe (Binds l))
 */
public class Match extends MatchTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public PatTopType[] pats;
    public RhsTopType rhs;
    public BindsTopType bindsMaybe;

    @Override
    public String toString() {
        return "Match{" +
                "name=" + name +
                ", pats=" + Arrays.toString(pats) +
                ", rhs=" + rhs +
                ", bindsMaybe=" + bindsMaybe +
                '}';
    }
}
