package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data IPBind l = IPBind l (IPName l) (Exp l)
 */
public class IPBind {
    public SrcInfoSpan srcInfoSpan;
    public IPNameTopType ipName;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "IPBind{" +
                "ipName=" + ipName +
                ", exp=" + exp +
                '}';
    }
}
