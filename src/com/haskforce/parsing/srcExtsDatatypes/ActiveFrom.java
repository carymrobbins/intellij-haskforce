package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ActiveFrom   l Int
 */
public class ActiveFrom extends ActivationTopType {
    public SrcInfoSpan srcInfoSpan;
    public int anInt;

    @Override
    public String toString() {
        return "ActiveFrom{" +
                anInt +
                '}';
    }
}
