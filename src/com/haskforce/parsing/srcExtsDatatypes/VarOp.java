package com.haskforce.parsing.srcExtsDatatypes;

/**
 * VarOp l (Name l)
 */
public class VarOp extends OpTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "VarOp{" +
                "name=" + name +
                '}';
    }
}
