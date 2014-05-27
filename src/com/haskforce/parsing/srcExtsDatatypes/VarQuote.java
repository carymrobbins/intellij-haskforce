package com.haskforce.parsing.srcExtsDatatypes;

/**
 * VarQuote l (QName l)
 */
public class VarQuote extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "VarQuote{" +
                qName +
                '}';
    }
}
