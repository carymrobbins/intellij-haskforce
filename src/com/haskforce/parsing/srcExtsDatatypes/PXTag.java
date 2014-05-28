package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PXTag l (XName l) [PXAttr l] (Maybe (Pat l)) [Pat l]
 */
public class PXTag extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public PXAttr[] pxAttrs;
    public PatTopType patMaybe;
    public PatTopType[] pats;

    @Override
    public String toString() {
        return "PXTag{" +
                "pats=" + Arrays.toString(pats) +
                ", patMaybe=" + patMaybe +
                ", pxAttrs=" + Arrays.toString(pxAttrs) +
                ", xName=" + xName +
                '}';
    }
}
