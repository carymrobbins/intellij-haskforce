package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PFieldPat l (QName l) (Pat l)
 */
public class PFieldPat extends PatFieldTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PFieldPat{" +
                "qName=" + qName +
                ", pat=" + pat +
                '}';
    }
}
