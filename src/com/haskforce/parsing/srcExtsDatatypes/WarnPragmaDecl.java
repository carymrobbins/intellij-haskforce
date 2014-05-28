package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 *  WarnPragmaDecl   l [([Name l], String)]
 */
public class WarnPragmaDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameStringPair[] pragmas;

    @Override
    public String toString() {
        return "WarnPragmaDecl{" +
                "pragmas=" + Arrays.toString(pragmas) +
                '}';
    }
}
