package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InfixA l (Type l) (QName l) (Type l)  -- ^ class assertion where the class name is given infix
 */
public class InfixA extends AsstTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public QNameTopType qName;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "InfixA{" +
                "t1=" + t1 +
                ", qName=" + qName +
                ", t2=" + t2 +
                '}';
    }
}
