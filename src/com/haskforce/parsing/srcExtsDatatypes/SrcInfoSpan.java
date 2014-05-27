package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Describes a source span info in the file. Corresponds to the Haskell datatype:
 *
 * data SrcSpanInfo = SrcSpanInfo
 *  { srcInfoSpan    :: SrcSpan
 *  , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
 *  }
 */
public class SrcInfoSpan {
    public SrcSpan srcInfoSpan;
    public SrcSpan[] srcInfoPoints;

    @Override
    public String toString() {
        return "SrcInfoSpan{" +
                "srcInfoSpan=" + srcInfoSpan +
                ", srcInfoPoints=" + Arrays.toString(srcInfoPoints) +
                '}';
    }
}
