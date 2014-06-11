package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IParam l (IPName l) (Type l)          -- ^ implicit parameter assertion
 */
public class IParam extends AsstTopType {
    public SrcInfoSpan srcInfoSpan;
    public IPNameTopType ipName;
    public TypeTopType type;

    @Override
    public String toString() {
        return "IParam{" +
                "ipName=" + ipName +
                ", type=" + type +
                '}';
    }
}
