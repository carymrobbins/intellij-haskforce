package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyInfix l (Type l) (QName l) (Type l)
 */
public class TyInfix extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public QNameTopType qName;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "TyInfix{" +
                "t1=" + t1 +
                ", qName=" + qName +
                ", t2=" + t2 +
                '}';
    }
}
