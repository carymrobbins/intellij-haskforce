package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnBangedTy   l (Type l)
 */
public class UnBangedTy extends BangTypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;

    @Override
    public String toString() {
        return "UnBangedTy{" +
                "type=" + type +
                '}';
    }
}
