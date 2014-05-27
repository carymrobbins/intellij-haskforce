package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PVar l (Name l)
 */
public class PVar extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "PVar{" +
                "name=" + name +
                '}';
    }
}
