package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ClsTyDef   l (Type l) (Type l)
 */
public class ClsTyDef extends ClassDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "ClsTyDef{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
