package com.haskforce.parsing.srcExtsDatatypes;

/**
 *data WarningText l
 = DeprText l String
 | WarnText l String
 */
public class WarnText extends WarningTextTopType {
    public SrcInfoSpan srcInfoSpan;
    public String text;

    @Override
    public String toString() {
        return "WarnText{" +
                "text='" + text + '\'' +
                '}';
    }
}
