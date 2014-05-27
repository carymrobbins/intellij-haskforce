package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  DeprPragmaDecl   l [([Name l], String)]
 */
public class DeprPragmaDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameStringPair[] pragmas;

    @Override
    public String toString() {
        return "DeprPragmaDecl{" +
                "pragmas=" + Arrays.toString(pragmas) +
                '}';
    }
}
