package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * DataInsDecl  l (DataOrNew l) (Type l)                  [QualConDecl l] (Maybe (Deriving l))
 */
public class DataInsDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public TypeTopType type;
    public QualConDecl[] qualConDecls;
    public Deriving derivingMaybe;

    @Override
    public String toString() {
        return "DataInsDecl{" +
                "derivingMaybe=" + derivingMaybe +
                ", qualConDecls=" + Arrays.toString(qualConDecls) +
                ", type=" + type +
                ", dataOrNew=" + dataOrNew +
                '}';
    }
}
