package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data XAttr l = XAttr l (XName l) (Exp l)
 */
public class XAttr {
    public SrcInfoSpan srcInfoSpan;
    public XNameTopType xName;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "XAttr{" +
                "xName=" + xName +
                ", exp=" + exp +
                '}';
    }
}
