package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ClassA l (QName l) [Type l]           -- ^ ordinary class assertion
 */
public class ClassA extends AsstTopType {
    public SrcInfoSpan srcInfoSpan;
    public QNameTopType qName;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "ClassA{" +
                "qName=" + qName +
                ", types=" + Arrays.toString(types) +
                '}';
    }
}
