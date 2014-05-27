package com.haskforce.parsing.srcExtsDatatypes;

/**
 * KindStar l
 */
public class KindStar extends KindTopType { // TODO: Deserialize?
    public SrcInfoSpan srcInfoSpan;

    @Override
    public String toString() {
        return "KindStar";
    }
}
