package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyList  l (Type l)
 */
public class TyList extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t;

    @Override
    public String toString() {
        return "TyList{" +
                t +
                '}';
    }
}
