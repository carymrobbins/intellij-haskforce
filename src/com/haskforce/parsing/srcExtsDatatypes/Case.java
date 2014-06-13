package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Case l (Exp l) [Alt l]
 */
public class Case extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType scrutinee;
    public Alt[] alts;

    @Override
    public String toString() {
        return "Case{" +
                "scrutinee=" + scrutinee +
                ", alts=" + Arrays.toString(alts) +
                '}';
    }
}
