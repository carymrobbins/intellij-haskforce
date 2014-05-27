package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  ConName l (Name l)
 */
public class ConName extends CNameTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "ConName{" +
                "name=" + name +
                '}';
    }
}
