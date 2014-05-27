package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PatBind      l (Pat l) (Maybe (Type l)) (Rhs l) {-where-} (Maybe (Binds l))
 */
public class PatBind extends DeclTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public TypeTopType type;
    public RhsTopType rhs;
    public BindsTopType binds;

    @Override
    public String toString() {
        return "PatBind{" +
                "binds=" + binds +
                ", rhs=" + rhs +
                ", type=" + type +
                ", pat=" + pat +
                '}';
    }
}
