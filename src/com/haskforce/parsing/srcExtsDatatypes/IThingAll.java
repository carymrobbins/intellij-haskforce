package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IThingAll l (Name l)
 */
public class IThingAll extends ImportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "IThingAll{" +
                "name=" + name +
                '}';
    }
}
