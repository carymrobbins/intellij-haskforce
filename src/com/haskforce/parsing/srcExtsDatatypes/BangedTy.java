package com.haskforce.parsing.srcExtsDatatypes;

/**
 * BangedTy   l (Type l)
 */
public class BangedTy extends BangTypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;

    @Override
    public String toString() {
        return "BangedTy{" +
                "type=" + type +
                '}';
    }
}
