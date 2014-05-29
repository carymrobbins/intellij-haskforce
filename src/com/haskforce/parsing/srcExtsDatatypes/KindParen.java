package com.haskforce.parsing.srcExtsDatatypes;

/**
 * KindParen l (Kind l)
 */
public class KindParen extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public KindTopType kind;

    @Override
    public String toString() {
        return "KindParen{" +
                kind +
                '}';
    }
}
