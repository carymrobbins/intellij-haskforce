package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ClsDecl    l (Decl l)
 */
public class ClsDecl extends ClassDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclTopType decl;

    @Override
    public String toString() {
        return "ClsDecl{" +
                "decl=" + decl +
                '}';
    }
}
