package com.haskforce.parsing.srcExtsDatatypes;

/**
 * XName l String
 */
public class XName extends XNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;

    @Override
    public String toString() {
        return "XName{" +
                '\'' + s + '\'' +
                '}';
    }
}
