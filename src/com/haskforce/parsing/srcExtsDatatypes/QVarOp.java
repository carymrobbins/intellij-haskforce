package com.haskforce.parsing.srcExtsDatatypes;

/**
 * QVarOp l (QName l)
 */
public class QVarOp extends QOpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "QVarOp{" +
                "qName=" + qName +
                '}';
    }
}
