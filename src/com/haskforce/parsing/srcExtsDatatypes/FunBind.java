package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * FunBind      l [Match l]
 */
public class FunBind extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public MatchTopType[] match;

    @Override
    public String toString() {
        return "FunBind{" +
                "match=" + Arrays.toString(match) +
                '}';
    }
}
