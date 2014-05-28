package com.haskforce.parsing.srcExtsDatatypes;

/**
 * TypedRuleVar l (Name l) (Type l)
 */
public class TypedRuleVar extends RuleVarTopType {
    public SrcInfoSpan srcInfoSpan;
    public NameTopType name;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TypedRuleVar{" +
                "name=" + name +
                ", type=" + type +
                '}';
    }
}
