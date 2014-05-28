package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PQuasiQuote l String String
 */
public class PQuasiQuote extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s1;
    public String s2;

    @Override
    public String toString() {
        return "PQuasiQuote{" +
                "s1='" + s1 + '\'' +
                ", s2='" + s2 + '\'' +
                '}';
    }
}
