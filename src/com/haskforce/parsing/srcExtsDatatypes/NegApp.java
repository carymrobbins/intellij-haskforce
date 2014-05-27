package com.haskforce.parsing.srcExtsDatatypes;

/**
 * NegApp l (Exp l)
 */
public class NegApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;

    @Override
    public String toString() {
        return "NegApp{" +
                e1 +
                '}';
    }
}
