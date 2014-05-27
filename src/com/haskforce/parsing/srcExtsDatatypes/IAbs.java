package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IAbs l (Name l)
 */
public class IAbs extends ImportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "IAbs{" +
                "name=" + name +
                '}';
    }
}
