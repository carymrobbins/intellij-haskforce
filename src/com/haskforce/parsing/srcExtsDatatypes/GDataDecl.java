package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  GDataDecl    l (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
 *          [GadtDecl l]    (Maybe (Deriving l))
 */
public class GDataDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public ContextTopType contextMaybe;
    public DeclHeadTopType declHead;
    public KindTopType kindMaybe;
    public GadtDecl[] gadtDecls;
    public Deriving derivingMaybe;

    @Override
    public String toString() {
        return "GDataDecl{" +
                "derivingMaybe=" + derivingMaybe +
                ", gadtDecls=" + Arrays.toString(gadtDecls) +
                ", kindMaybe=" + kindMaybe +
                ", declHead=" + declHead +
                ", contextMaybe=" + contextMaybe +
                ", dataOrNew=" + dataOrNew +
                '}';
    }
}
