package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PNPlusK l (Name l) Integer
 */
public class PNPlusK extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public Integer i;

    @Override
    public String toString() {
        return "PNPlusK{" +
                "name=" + name +
                ", i=" + i +
                '}';
    }
}
