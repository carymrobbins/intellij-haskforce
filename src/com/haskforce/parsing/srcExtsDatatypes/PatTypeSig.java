package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PatTypeSig l (Pat l) (Type l)
 */
public class PatTypeSig extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public TypeTopType type;

    @Override
    public String toString() {
        return "PatTypeSig{" +
                "pat=" + pat +
                ", type=" + type +
                '}';
    }
}
