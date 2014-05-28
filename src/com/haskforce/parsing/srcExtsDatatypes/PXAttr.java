package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data PXAttr l = PXAttr l (XName l) (Pat l)
 */
public class PXAttr {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PXAttr{" +
                "xName=" + xName +
                ", pat=" + pat +
                '}';
    }
}
