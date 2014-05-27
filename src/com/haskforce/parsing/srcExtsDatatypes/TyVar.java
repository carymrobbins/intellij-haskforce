package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyVar   l (Name l)
 */
public class TyVar extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "TyVar{" +
                "name=" + name +
                '}';
    }
}
