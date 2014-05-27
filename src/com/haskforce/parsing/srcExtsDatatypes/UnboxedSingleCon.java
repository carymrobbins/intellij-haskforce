package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnboxedSingleCon l    -- ^ unboxed singleton tuple constructor @(\# \#)@
 */
public class UnboxedSingleCon extends SpecialConTopType {
    public SrcInfoSpan srcInfoSpan;

    @Override
    public String toString() {
        return "UnboxedSingleCon{}";
    }
}
