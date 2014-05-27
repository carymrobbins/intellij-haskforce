package com.haskforce.parsing.srcExtsDatatypes;

/**
 * QConOp l (QName l)
 */
public class QConOp extends QOpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "QConOp{" +
                "qName=" + qName +
                '}';
    }
}
