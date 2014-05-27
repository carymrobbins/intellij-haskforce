package com.haskforce.parsing.srcExtsDatatypes;

/**
 * FieldPun l (Name l)
 */
public class FieldPun extends FieldUpdateTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "FieldPun{" +
                "name=" + name +
                '}';
    }
}
