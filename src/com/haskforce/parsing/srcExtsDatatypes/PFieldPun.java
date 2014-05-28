package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PFieldPun l (Name l)
 */
public class PFieldPun extends PatFieldTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "PFieldPun{" +
                "name=" + name +
                '}';
    }
}
