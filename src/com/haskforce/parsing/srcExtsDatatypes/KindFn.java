package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  KindFn    l (Kind l) (Kind l)
 */
public class KindFn extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public KindTopType k1;
    public KindTopType k2;

    @Override
    public String toString() {
        return "KindFn{" +
                "k1=" + k1 +
                ", k2=" + k2 +
                '}';
    }
}
