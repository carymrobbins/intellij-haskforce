package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * DefaultDecl  l [Type l]
 */
public class DefaultDecl extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "DefaultDecl{" +
                "types=" + Arrays.toString(types) +
                '}';
    }
}
