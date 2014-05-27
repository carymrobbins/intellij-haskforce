package com.haskforce.parsing.srcExtsDatatypes;

/**
 * KindedVar   l (Name l) (Kind l)
 */
public class KindedVar extends TyVarBindTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public KindTopType kind;

    @Override
    public String toString() {
        return "KindedVar{" +
                "name=" + name +
                ", kind=" + kind +
                '}';
    }
}
