package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypQuote l (QName l)
 */
public class TypQuote extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "TypQuote{" +
                qName +
                '}';
    }
}
