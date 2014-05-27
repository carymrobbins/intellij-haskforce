package com.haskforce.parsing.srcExtsDatatypes;

/**
 * If l (Exp l) (Exp l) (Exp l)
 */
public class If extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType cond;
    public ExpTopType t;
    public ExpTopType f;

    @Override
    public String toString() {
        return "If{" +
                "cond=" + cond +
                ", t=" + t +
                ", f=" + f +
                '}';
    }
}
