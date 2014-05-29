package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RPAs l (Name l) (RPat l)
 */
public class RPAs extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public RPatTopType rpat;

    @Override
    public String toString() {
        return "RPAs{" +
                "name=" + name +
                ", rpat=" + rpat +
                '}';
    }
}
