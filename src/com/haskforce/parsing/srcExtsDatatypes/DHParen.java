package com.haskforce.parsing.srcExtsDatatypes;

/**
 * DHParen l (DeclHead l)
 */
public class DHParen extends DeclHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclHeadTopType declHead;

    @Override
    public String toString() {
        return "DHParen{" +
                "declHead=" + declHead +
                '}';
    }
}
