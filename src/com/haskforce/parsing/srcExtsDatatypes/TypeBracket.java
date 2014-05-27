package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  TypeBracket l (Type l)
 */
public class TypeBracket extends BracketTopType {
    public SrcInfoSpan srcInfoSpan;
    public TypeTopType type;

    @Override
    public String toString() {
        return "TypeBracket{" +
                "type=" + type +
                '}';
    }
}
