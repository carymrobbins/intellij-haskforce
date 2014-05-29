package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  KindApp   l (Kind l) (Kind l)
 */
public class KindApp extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public KindTopType k1;
    public KindTopType k2;

    @Override
    public String toString() {
        return "KindApp{" +
                "k1=" + k1 +
                ", k2=" + k2 +
                '}';
    }
}
