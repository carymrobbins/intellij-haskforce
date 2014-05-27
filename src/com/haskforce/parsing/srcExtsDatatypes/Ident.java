package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Ident  l String
 */
public class Ident extends NameTopType {
    public SrcInfoSpan srcInfoSpan;
    public String name;

    @Override
    public String toString() {
        return "'" + name + "'";
    }
}
