package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data ModuleName l = ModuleName l String
 */
public class ModuleName {
    public SrcInfoSpan srcInfoSpan;
    public String name;

    @Override
    public String toString() {
        return "ModuleName{" +
                "\'" + name + '\'' +
                '}';
    }
}
