package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RPCAs l (Name l) (RPat l)
 */
public class RPCAs extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public RPatTopType rpat;

    @Override
    public String toString() {
        return "RPCAs{" +
                "name=" + name +
                ", rpat=" + rpat +
                '}';
    }
}
