package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnkindedVar l (Name l)
 */
public class UnkindedVar extends TyVarBindTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "UnkindedVar{" +
                "name=" + name +
                '}';
    }
}
