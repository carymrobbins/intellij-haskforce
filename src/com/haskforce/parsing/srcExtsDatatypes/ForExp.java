package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ForExp       l (CallConv l)                    (Maybe String) (Name l) (Type l)
 */
public class ForExp extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public CallConvTopType callConv;
    public String stringMaybe;
    public NameTopType name;
    public TypeTopType type;

    @Override
    public String toString() {
        return "ForExp{" +
                "type=" + type +
                ", name=" + name +
                ", stringMaybe='" + stringMaybe + '\'' +
                ", callConv=" + callConv +
                '}';
    }
}
