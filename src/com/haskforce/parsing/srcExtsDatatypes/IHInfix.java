package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IHInfix l (Type l) (QName l) (Type l)
 */
public class IHInfix extends InstHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public QNameTopType qName;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "IHInfix{" +
                "t1=" + t1 +
                ", qName=" + qName +
                ", t2=" + t2 +
                '}';
    }
}
