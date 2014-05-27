package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypeDecl     l (DeclHead l) (Type l)
 */
public class TypeDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclHeadTopType declHead;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TypeDecl{" +
                "declHead=" + declHead +
                ", type=" + type +
                '}';
    }
}
