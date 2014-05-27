package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * XETag l (XName l) [XAttr l] (Maybe (Exp l))
 */
public class XETag extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public XAttr[] xAttrs;
    public ExpTopType expMaybe;

    @Override
    public String toString() {
        return "XETag{" +
                "xName=" + xName +
                ", xAttrs=" + Arrays.toString(xAttrs) +
                ", expMaybe=" + expMaybe +
                '}';
    }
}
