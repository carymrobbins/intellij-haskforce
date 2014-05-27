package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
 */
public class XTag extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public XAttr[] xAttrs;
    public ExpTopType expMaybe;
    public ExpTopType[] exps;

    @Override
    public String toString() {
        return "XTag{" +
                "exps=" + Arrays.toString(exps) +
                ", expMaybe=" + expMaybe +
                ", xAttrs=" + Arrays.toString(xAttrs) +
                ", xName=" + xName +
                '}';
    }
}
