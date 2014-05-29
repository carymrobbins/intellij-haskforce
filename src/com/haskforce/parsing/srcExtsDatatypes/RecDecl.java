package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RecDecl l (Name l) [FieldDecl l]
 */
public class RecDecl extends ConDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public FieldDecl[] fields;

    @Override
    public String toString() {
        return "RecDecl{" +
                "name=" + name +
                ", fields=" + Arrays.toString(fields) +
                '}';
    }
}
