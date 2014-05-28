package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data GadtDecl l = GadtDecl l (Name l) (Type l)
 */
public class GadtDecl {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public TypeTopType type;

    @Override
    public String toString() {
        return "GadtDecl{" +
                "name=" + name +
                ", type=" + type +
                '}';
    }
}
