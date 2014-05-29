package com.haskforce.parsing.srcExtsDatatypes;

/**
 * DHInfix l (TyVarBind l) (Name l) (TyVarBind l)
 */
public class DHInfix extends DeclHeadTopType {
    public SrcInfoSpan srcInfoSpan;
    public TyVarBindTopType tb1;
    public NameTopType name;
    public TyVarBindTopType tb2;

    @Override
    public String toString() {
        return "DHInfix{" +
                "tb1=" + tb1 +
                ", name=" + name +
                ", tb2=" + tb2 +
                '}';
    }
}
