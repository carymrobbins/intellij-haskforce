package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * EThingWith l (QName l) [CName l]
 */
public class EThingWith extends ExportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public CNameTopType[] cNames;

    @Override
    public String toString() {
        return "EThingWith{" +
                "qName=" + qName +
                ", cNames=" + Arrays.toString(cNames) +
                '}';
    }
}
