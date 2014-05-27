package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l)  [QualConDecl l] (Maybe (Deriving l))
 */
public class DataDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DataOrNewTopType dataOrNew;
    public ContextTopType contextMaybe;
    public DeclHeadTopType declHead;
    public QualConDecl[] qualConDecls;
    public Deriving deriving;

    @Override
    public String toString() {
        return "DataDecl{" +
                "dataOrNew=" + dataOrNew +
                ", contextMaybe=" + contextMaybe +
                ", declHead=" + declHead +
                ", qualConDecls=" + Arrays.toString(qualConDecls) +
                ", deriving=" + deriving +
                '}';
    }
}
