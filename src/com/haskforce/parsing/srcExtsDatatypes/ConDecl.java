package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ConDecl l (Name l) [BangType l]
 */
public class ConDecl extends ConDeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public BangTypeTopType[] bangTypes;

    @Override
    public String toString() {
        return "ConDecl{" +
                "name=" + name +
                ", bangTypes=" + Arrays.toString(bangTypes) +
                '}';
    }
}
