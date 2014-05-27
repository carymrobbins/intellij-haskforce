package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyFun   l (Type l) (Type l)
 */
public class TyFun extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "TyFun{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
