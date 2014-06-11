package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EqualP l (Type l) (Type l)            -- ^ type equality constraint
 */
public class EqualP extends AsstTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "EqualP{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
