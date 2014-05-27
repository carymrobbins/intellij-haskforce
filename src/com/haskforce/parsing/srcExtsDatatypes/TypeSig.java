package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * TypeSig      l [Name l] (Type l)
 */
public class TypeSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType[] names;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TypeSig{" +
                "names=" + Arrays.toString(names) +
                ", type=" + type +
                '}';
    }
}
