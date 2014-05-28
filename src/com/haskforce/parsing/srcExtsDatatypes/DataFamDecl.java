package com.haskforce.parsing.srcExtsDatatypes;

/**
 * DataFamDecl  l {-data-}      (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
 */
public class DataFamDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public DeclHeadTopType declHead;
    public KindTopType kindMaybe;

    @Override
    public String toString() {
        return "DataFamDecl{" +
                "kindMaybe=" + kindMaybe +
                ", declHead=" + declHead +
                ", contextMaybe=" + contextMaybe +
                '}';
    }
}
