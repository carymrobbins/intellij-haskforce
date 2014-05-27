package com.haskforce.parsing.srcExtsDatatypes;

/**
 * DeprText l String
 */
public class DeprText extends WarningTextTopType {
    public SrcInfoSpan srcInfoSpan;
    public String text;

    @Override
    public String toString() {
        return "DeprText{" +
                "text='" + text + '\'' +
                '}';
    }
}
