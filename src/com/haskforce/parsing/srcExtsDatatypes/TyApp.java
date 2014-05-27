package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyApp   l (Type l) (Type l)
 */
public class TyApp extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "TyApp{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
