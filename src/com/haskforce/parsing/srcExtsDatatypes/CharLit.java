package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Char       l Char     String
 */
public class CharLit extends LiteralTopType {
    public SrcInfoSpan srcInfoSpan;
    public char value;
    public String representation;

    @Override
    public String toString() {
        return "CharLit{" +
                "value=" + value +
                ", representation='" + representation + '\'' +
                '}';
    }
}
