package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * data FieldDecl l = FieldDecl l [Name l] (BangType l)
 */
public class FieldDecl {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType[] names;
    public BangTypeTopType bang;

    @Override
    public String toString() {
        return "FieldDecl{" +
                "names=" + Arrays.toString(names) +
                ", bang=" + bang +
                '}';
    }
}
