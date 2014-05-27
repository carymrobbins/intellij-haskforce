package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyKind  l (Type l) (Kind l)
 */
public class TyKind extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;
    public KindTopType kind;

    @Override
    public String toString() {
        return "TyKind{" +
                "type=" + type +
                ", kind=" + kind +
                '}';
    }
}
