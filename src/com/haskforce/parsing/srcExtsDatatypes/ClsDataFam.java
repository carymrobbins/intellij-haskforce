package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
 */
public class ClsDataFam extends ClassDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public DeclHeadTopType declHead;
    public KindTopType kindMaybe;

    @Override
    public String toString() {
        return "ClsDataFam{" +
                "contextMaybe=" + contextMaybe +
                ", declHead=" + declHead +
                ", kindMaybe=" + kindMaybe +
                '}';
    }
}
