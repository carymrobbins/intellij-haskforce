package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EThingAll l (QName l)
 */
public class EThingAll extends ExportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "EThingAll{" +
                "qName=" + qName +
                '}';
    }
}
