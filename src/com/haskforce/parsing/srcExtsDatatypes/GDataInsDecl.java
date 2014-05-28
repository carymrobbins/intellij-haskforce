package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * GDataInsDecl l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
 */
public class GDataInsDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public TypeTopType type;
    public KindTopType kindMaybe;
    public GadtDecl[] gadtDecls;
    public Deriving derivingMaybe;

    @Override
    public String toString() {
        return "GDataInsDecl{" +
                "derivingMaybe=" + derivingMaybe +
                ", gadtDecls=" + Arrays.toString(gadtDecls) +
                ", kindMaybe=" + kindMaybe +
                ", type=" + type +
                ", dataOrNew=" + dataOrNew +
                '}';
    }
}
