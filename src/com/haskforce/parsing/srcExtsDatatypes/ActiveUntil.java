package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ActiveUntil  l Int
 */
public class ActiveUntil extends ActivationTopType {
    public SrcInfoSpan srcInfoSpan;
    public int anInt;

    @Override
    public String toString() {
        return "ActiveUntil{" +
                "anInt=" + anInt +
                '}';
    }
}
