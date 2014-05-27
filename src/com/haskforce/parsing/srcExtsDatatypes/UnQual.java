package com.haskforce.parsing.srcExtsDatatypes;

/**
 * UnQual  l                (Name l)
 */
public class UnQual extends QNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return name.toString();
    }
}
