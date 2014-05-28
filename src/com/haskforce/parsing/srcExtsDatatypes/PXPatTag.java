package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PXPatTag l (Pat l)
 */
public class PXPatTag extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PXPatTag{" +
                "pat=" + pat +
                '}';
    }
}
