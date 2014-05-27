package com.haskforce.parsing.srcExtsDatatypes;

/**
 * QuasiQuote l String String
 */
public class QuasiQuote extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s1;
    public String s2;

    @Override
    public String toString() {
        return "QuasiQuote{" +
                "s1='" + s1 + '\'' +
                ", s2='" + s2 + '\'' +
                '}';
    }
}
