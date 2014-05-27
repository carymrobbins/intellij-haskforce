package com.haskforce.parsing.srcExtsDatatypes;

/**
 * IdSplice l String
 */
public class IdSplice extends SpliceTopType {
    public SrcInfoSpan srcInfoSpan;
    public String s;

    @Override
    public String toString() {
        return "IdSplice{" +
                "s='" + s + '\'' +
                '}';
    }
}
