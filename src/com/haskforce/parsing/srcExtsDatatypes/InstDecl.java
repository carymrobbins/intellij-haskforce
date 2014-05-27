package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * InstDecl     l (Maybe (Context l)) (InstHead l) (Maybe [InstDecl l])
 */
public class InstDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public InstHeadTopType instHead;
    public InstDeclTopType[] instDecls;

    @Override
    public String toString() {
        return "InstDecl{" +
                ", contextMaybe=" + contextMaybe +
                ", instHead=" + instHead +
                ", instDecls=" + Arrays.toString(instDecls) +
                '}';
    }
}
