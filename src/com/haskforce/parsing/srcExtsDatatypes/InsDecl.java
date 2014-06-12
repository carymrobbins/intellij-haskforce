package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InsDecl   l (Decl l)
 */
public class InsDecl extends InstDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclTopType decl;

    @Override
    public String toString() {
        return "InsDecl{" +
                "decl=" + decl +
                '}';
    }
}
