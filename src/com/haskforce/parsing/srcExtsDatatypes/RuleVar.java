package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RuleVar l (Name l)
 */
public class RuleVar extends RuleVarTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;

    @Override
    public String toString() {
        return "RuleVar{" +
                "name=" + name +
                '}';
    }
}
