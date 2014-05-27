package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IVar l (Name l)
 */
public class IVar extends ImportSpecTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "IVar{" +
                "name=" + name +
                '}';
    }
}
