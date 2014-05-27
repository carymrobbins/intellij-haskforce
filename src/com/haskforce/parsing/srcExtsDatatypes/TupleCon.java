package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TupleCon l Boxed Int
 */
public class TupleCon extends SpecialConTopType {
    public SrcInfoSpan srcInfoSpan;
    public BoxedTopType boxed;
    public int i;

    @Override
    public String toString() {
        return "TupleCon{" +
                "boxed=" + boxed +
                ", i=" + i +
                '}';
    }
}
