package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) {-where-} (Maybe (Binds l))
 */
public class InfixMatch extends MatchTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public NameTopType name;
    public PatTopType[] pats;
    public RhsTopType rhs;
    public BindsTopType bindsMaybe;

    @Override
    public String toString() {
        return "InfixMatch{" +
                "bindsMaybe=" + bindsMaybe +
                ", rhs=" + rhs +
                ", pats=" + Arrays.toString(pats) +
                ", name=" + name +
                ", pat=" + pat +
                '}';
    }
}
