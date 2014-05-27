package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * IThingWith l (Name l) [CName l]
 */
public class IThingWith extends ImportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public CNameTopType[] cnames;

    @Override
    public String toString() {
        return "IThingWith{" +
                "name=" + name +
                ", cnames=" + Arrays.toString(cnames) +
                '}';
    }
}
