package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * GuardedAlts  l [GuardedAlt l]
 */
public class GuardedAlts extends GuardedAltsTopType {
    public SrcInfoSpan srcInfoSpan;
    public GuardedAlt[] alts;

    @Override
    public String toString() {
        return "GuardedAlts{" +
                "alts=" + Arrays.toString(alts) +
                '}';
    }
}
