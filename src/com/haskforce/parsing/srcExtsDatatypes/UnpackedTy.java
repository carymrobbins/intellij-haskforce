package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnpackedTy   l (Type l)
 */
public class UnpackedTy extends BangTypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;

    @Override
    public String toString() {
        return "UnpackedTy{" +
                "type=" + type +
                '}';
    }
}
