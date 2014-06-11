package com.haskforce.parsing.srcExtsDatatypes;

/**
 * CxParen  l (Context l)
 */
public class CxParen extends ContextTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType context;

    @Override
    public String toString() {
        return "CxParen(" +
                context +
                ')';
    }
}
