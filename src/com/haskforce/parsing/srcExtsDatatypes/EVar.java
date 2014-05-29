package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EVar l (QName l)
 */
public class EVar extends ExportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "EVar{" +
                "qName=" + qName +
                '}';
    }
}
