package com.haskforce.parsing.srcExtsDatatypes;

/**
 * XDomName l String String
 */
public class XDomName extends XNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s1;
    public String s2;

    @Override
    public String toString() {
        return "XDomName{" +
                "s1='" + s1 + '\'' +
                ", s2='" + s2 + '\'' +
                '}';
    }
}
