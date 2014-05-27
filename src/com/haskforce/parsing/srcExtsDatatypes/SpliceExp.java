package com.haskforce.parsing.srcExtsDatatypes;

/**
 * SpliceExp l (Splice l)
 */
public class SpliceExp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public SpliceTopType splice;

    @Override
    public String toString() {
        return "SpliceExp{" +
                "splice=" + splice +
                '}';
    }
}
