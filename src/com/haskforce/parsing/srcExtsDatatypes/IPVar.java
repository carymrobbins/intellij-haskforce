package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IPVar l (IPName l)
 */
public class IPVar extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public IPNameTopType ipName;

    @Override
    public String toString() {
        return "IPVar{" +
                "ipName=" + ipName +
                '}';
    }
}
