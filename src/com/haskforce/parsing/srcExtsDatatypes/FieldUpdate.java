package com.haskforce.parsing.srcExtsDatatypes;

/**
 * FieldUpdate l (QName l) (Exp l)
 */
public class FieldUpdate extends FieldUpdateTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "FieldUpdate{" +
                "qName=" + qName +
                ", exp=" + exp +
                '}';
    }
}
