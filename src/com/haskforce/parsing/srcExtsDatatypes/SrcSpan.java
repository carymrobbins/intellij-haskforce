package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Describes a source span in the file.
 *
 * data SrcSpan = SrcSpan
 * { srcSpanFilename    :: String
 * , srcSpanStartLine   :: Int
 * , srcSpanStartColumn :: Int
 * , srcSpanEndLine     :: Int
 * , srcSpanEndColumn   :: Int
 * }
 */
public class SrcSpan {
    public String srcSpanFilename;
    public int srcSpanStartLine;
    public int srcSpanStartColumn;
    public int srcSpanEndLine;
    public int srcSpanEndColumn;

    @Override
    public String toString() {
        return "SrcSpan{" +
                "srcSpanFilename='" + srcSpanFilename + '\'' +
                ", srcSpanStartLine=" + srcSpanStartLine +
                ", srcSpanStartColumn=" + srcSpanStartColumn +
                ", srcSpanEndLine=" + srcSpanEndLine +
                ", srcSpanEndColumn=" + srcSpanEndColumn +
                '}';
    }
}
