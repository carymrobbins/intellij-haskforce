package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  VarName l (Name l)
 */
public class VarName extends CNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "VarName{" +
                "name=" + name +
                '}';
    }
}
