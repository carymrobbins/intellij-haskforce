package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RecUpdate l (Exp l)   [FieldUpdate l]
 */
public class RecUpdate extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public FieldUpdateTopType[] fieldUpdates;

    @Override
    public String toString() {
        return "RecUpdate{" +
                "exp=" + exp +
                ", fieldUpdates=" + Arrays.toString(fieldUpdates) +
                '}';
    }
}
