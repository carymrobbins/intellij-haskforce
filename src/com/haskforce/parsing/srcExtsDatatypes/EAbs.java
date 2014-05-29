package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EAbs l (QName l)
 */
public class EAbs extends ExportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "EAbs{" +
                "qName=" + qName +
                '}';
    }
}
