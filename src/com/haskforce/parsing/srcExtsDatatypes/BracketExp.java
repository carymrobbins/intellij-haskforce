package com.haskforce.parsing.srcExtsDatatypes;

/**
 * BracketExp l (Bracket l)
 */
public class BracketExp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public BracketTopType bracket;

    @Override
    public String toString() {
        return "BracketExp{" +
                "bracket=" + bracket +
                '}';
    }
}
