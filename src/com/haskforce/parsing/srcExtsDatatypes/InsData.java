package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * InsData   l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l))
 */
public class InsData extends InstDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public TypeTopType type;
    public QualConDecl[] qualConDecls;
    public Deriving derivingMaybe;

    @Override
    public String toString() {
        return "InsData{" +
                "derivingMaybe=" + derivingMaybe +
                ", qualConDecls=" + Arrays.toString(qualConDecls) +
                ", type=" + type +
                ", dataOrNew=" + dataOrNew +
                '}';
    }
}
