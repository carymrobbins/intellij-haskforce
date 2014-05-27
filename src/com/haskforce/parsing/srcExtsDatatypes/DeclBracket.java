package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  DeclBracket l [Decl l]
 */
public class DeclBracket extends BracketTopType {
    public SrcInfoSpan srcInfoSpan;
    public DeclTopType[] decls;

    @Override
    public String toString() {
        return "DeclBracket{" +
                "decls=" + Arrays.toString(decls) +
                '}';
    }
}
