package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PAsPat l (Name l) (Pat l)
 */
public class PAsPat extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PAsPat{" +
                "name=" + name +
                ", pat=" + pat +
                '}';
    }
}
