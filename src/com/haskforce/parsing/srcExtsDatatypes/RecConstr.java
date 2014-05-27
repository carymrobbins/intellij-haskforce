package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RecConstr l (QName l) [FieldUpdate l]
 */
public class RecConstr extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public FieldUpdateTopType[] fieldUpdates;

    @Override
    public String toString() {
        return "RecConstr{" +
                "qName=" + qName +
                ", fieldUpdates=" + Arrays.toString(fieldUpdates) +
                '}';
    }
}
