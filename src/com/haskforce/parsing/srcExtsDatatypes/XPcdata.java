package com.haskforce.parsing.srcExtsDatatypes;

/**
 * XPcdata l String
 */
public class XPcdata extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;

    @Override
    public String toString() {
        return "XPcdata{" +
                "s='" + s + '\'' +
                '}';
    }
}
