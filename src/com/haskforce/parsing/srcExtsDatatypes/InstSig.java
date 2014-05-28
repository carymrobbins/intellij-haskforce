package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InstSig          l      (Maybe (Context l))    (InstHead l)
 */
public class InstSig extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public ContextTopType contextMaybe;
    public InstHeadTopType instHead;

    @Override
    public String toString() {
        return "InstSig{" +
                "contextMaybe=" + contextMaybe +
                ", instHead=" + instHead +
                '}';
    }
}
