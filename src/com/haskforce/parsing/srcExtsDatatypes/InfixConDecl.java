package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InfixConDecl l (BangType l) (Name l) (BangType l)
 */
public class InfixConDecl extends ConDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public BangTypeTopType b1;
    public NameTopType name;
    public BangTypeTopType b2;

    @Override
    public String toString() {
        return "InfixConDecl{" +
                "b2=" + b2 +
                ", name=" + name +
                ", b1=" + b1 +
                '}';
    }
}
