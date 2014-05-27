package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ForImp       l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l)
 */
public class ForImp extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public CallConvTopType callConv;
    public SafetyTopType safety;
    public String s;
    public NameTopType name;
    public TypeTopType type;

    @Override
    public String toString() {
        return "ForImp{" +
                "callConv=" + callConv +
                ", safety=" + safety +
                ", s='" + s + '\'' +
                ", name=" + name +
                ", type=" + type +
                '}';
    }
}
