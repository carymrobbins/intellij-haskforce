package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  PromotedCon l Bool (QName l)
 */
public class PromotedCon extends PromotedTopType {
    public SrcInfoSpan srcInfoSpan;
    public boolean value;
    public QNameTopType qName;

    @Override
    public String toString() {
        return "PromotedCon{" +
                "value=" + value +
                ", qName=" + qName +
                '}';
    }
}
