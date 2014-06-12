package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IHParen l (InstHead l)
 */
public class IHParen extends InstHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public InstHeadTopType instHead;

    @Override
    public String toString() {
        return "IHParen{" +
                "instHead=" + instHead +
                '}';
    }
}
