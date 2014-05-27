package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * BDecls  l [Decl l]
 */
public class BDecls extends BindsTopType { // TODO: Unused?
    public SrcInfoSpan srcInfoSpan;
    public DeclTopType[] decls;

    @Override
    public String toString() {
        return "BDecls{" +
                "decls=" + Arrays.toString(decls) +
                '}';
    }
}
