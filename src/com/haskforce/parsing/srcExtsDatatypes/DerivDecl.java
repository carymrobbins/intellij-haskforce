package com.haskforce.parsing.srcExtsDatatypes;

/**
 * DerivDecl    l (Maybe (Context l)) (InstHead l)
 */
public class DerivDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public InstHeadTopType instHead;

    @Override
    public String toString() {
        return "DerivDecl{" +
                "contextMaybe=" + contextMaybe +
                ", instHead=" + instHead +
                '}';
    }
}
