package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Var l (QName l)
 */
public class Var extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "Var(" + qName + ')';
    }
}
