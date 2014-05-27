package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PatBracket l (Pat l)
 */
public class PatBracket extends BracketTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PatBracket{" +
                "pat=" + pat +
                '}';
    }
}
