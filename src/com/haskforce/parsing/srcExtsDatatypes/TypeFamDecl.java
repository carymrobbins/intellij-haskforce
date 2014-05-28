package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypeFamDecl  l (DeclHead l) (Maybe (Kind l))
 */
public class TypeFamDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclHeadTopType declHead;
    public KindTopType kindMaybe;

    @Override
    public String toString() {
        return "TypeFamDecl{" +
                "declHead=" + declHead +
                ", kindMaybe=" + kindMaybe +
                '}';
    }
}
