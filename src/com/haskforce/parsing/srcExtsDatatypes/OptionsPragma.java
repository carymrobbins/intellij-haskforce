package com.haskforce.parsing.srcExtsDatatypes;

/**
 * OptionsPragma    l (Maybe Tool) String
 */
public class OptionsPragma extends ModulePragmaTopType {
    public SrcInfoSpan srcInfoSpan;
    public ToolTopType tool;
    public String s;

    @Override
    public String toString() {
        return "OptionsPragma{" +
                "tool=" + tool +
                ", s='" + s + '\'' +
                '}';
    }
}
