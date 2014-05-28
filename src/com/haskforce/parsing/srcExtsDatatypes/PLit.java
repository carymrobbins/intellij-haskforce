package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  PLit l (Literal l)
 */
public class PLit extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public LiteralTopType lit;

    @Override
    public String toString() {
        return "PLit{" +
                "lit=" + lit +
                '}';
    }
}
