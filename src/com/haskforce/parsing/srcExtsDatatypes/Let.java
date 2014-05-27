package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Let l (Binds l) (Exp l)
 */
public class Let extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public BindsTopType binds;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Let{" +
                "binds=" + binds +
                ", exp=" + exp +
                '}';
    }
}
