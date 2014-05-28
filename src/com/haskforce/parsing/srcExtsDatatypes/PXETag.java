package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PXETag l (XName l) [PXAttr l] (Maybe (Pat l))
 */
public class PXETag extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public PXAttr[] pxAttrs;
    public PatTopType patMaybe;

    @Override
    public String toString() {
        return "PXETag{" +
                "patMaybe=" + patMaybe +
                ", pxAttrs=" + Arrays.toString(pxAttrs) +
                ", xName=" + xName +
                '}';
    }
}
