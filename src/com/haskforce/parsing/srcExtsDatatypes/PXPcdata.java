package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PXPcdata l String
 */
public class PXPcdata extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;

    @Override
    public String toString() {
        return "PXPcdata{" +
                '\'' + s + '\'' +
                '}';
    }
}
