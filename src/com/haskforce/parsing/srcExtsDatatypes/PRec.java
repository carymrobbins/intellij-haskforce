package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PRec l (QName l) [PatField l]
 */
public class PRec extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public PatFieldTopType[] patFields;

    @Override
    public String toString() {
        return "PRec{" +
                "qName=" + qName +
                ", patFields=" + Arrays.toString(patFields) +
                '}';
    }
}
