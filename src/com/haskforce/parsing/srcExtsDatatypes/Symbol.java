package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Symbol l String
 */
public class Symbol extends NameTopType {
    public SrcInfoSpan srcInfoSpan;
    public String symbol;

    @Override
    public String toString() {
        return "Symbol{" +
                "symbol='" + symbol + '\'' +
                '}';
    }
}
