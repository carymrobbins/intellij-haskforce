package com.haskforce.parsing.srcExtsDatatypes;

/**
 * App l (Exp l) (Exp l)
 */
public class App extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "App{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
