package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InsType   l (Type l) (Type l)
 */
public class InsType extends InstDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType t1;
    public TypeTopType t2;

    @Override
    public String toString() {
        return "InsType{" +
                "t1=" + t1 +
                ", t2=" + t2 +
                '}';
    }
}
