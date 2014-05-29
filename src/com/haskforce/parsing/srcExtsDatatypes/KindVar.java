package com.haskforce.parsing.srcExtsDatatypes;

/**
 * KindVar   l (QName l)
 */
public class KindVar extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "KindVar{" +
                "qName=" + qName +
                '}';
    }
}
