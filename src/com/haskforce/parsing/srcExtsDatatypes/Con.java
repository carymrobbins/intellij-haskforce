package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Con l (QName l)
 */
public class Con extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "Con{" +
                "qName=" + qName +
                '}';
    }
}
