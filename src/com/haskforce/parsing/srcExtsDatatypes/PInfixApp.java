package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PInfixApp l (Pat l) (QName l) (Pat l)
 */
public class PInfixApp extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType p1;
    public QNameTopType qName;
    public PatTopType p2;

    @Override
    public String toString() {
        return "PInfixApp{" +
                "p2=" + p2 +
                ", qName=" + qName +
                ", p1=" + p1 +
                '}';
    }
}
