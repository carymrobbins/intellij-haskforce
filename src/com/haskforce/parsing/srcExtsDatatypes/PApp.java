package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PApp l (QName l) [Pat l]
 */
public class PApp extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public PatTopType[] pats;

    @Override
    public String toString() {
        return "PApp{" +
                "qName=" + qName +
                ", pats=" + Arrays.toString(pats) +
                '}';
    }
}
