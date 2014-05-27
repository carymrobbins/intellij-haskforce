package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Special l (SpecialCon l)
 */
public class Special extends QNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public SpecialConTopType specialCon;

    @Override
    public String toString() {
        return "Special{" +
                "specialCon=" + specialCon +
                '}';
    }
}
