package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ConOp l (Name l)
 */
public class ConOp extends OpTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "ConOp{" +
                "name=" + name +
                '}';
    }
}
