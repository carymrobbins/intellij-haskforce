package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyParen l (Type l)
 */
public class TyParen extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TyParen{" +
                "type=" + type +
                '}';
    }
}
