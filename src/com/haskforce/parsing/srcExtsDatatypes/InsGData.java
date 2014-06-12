package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l))
 */
public class InsGData extends InstDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public TypeTopType type;
    public KindTopType kindMaybe;
    public GadtDecl[] gadtDecls;
    public Deriving derivingMaybe;

    @Override
    public String toString() {
        return "InsGData{" +
                "derivingMaybe=" + derivingMaybe +
                ", gadtDecls=" + Arrays.toString(gadtDecls) +
                ", kindMaybe=" + kindMaybe +
                ", type=" + type +
                ", dataOrNew=" + dataOrNew +
                '}';
    }
}
