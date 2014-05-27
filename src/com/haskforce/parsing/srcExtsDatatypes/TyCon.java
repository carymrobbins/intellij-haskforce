package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TyCon   l (QName l)
 */
public class TyCon extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "TyCon{" +
                "qName=" + qName +
                '}';
    }
}
