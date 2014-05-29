package com.haskforce.parsing.srcExtsDatatypes;

/**
 * LetStmt l (Binds l)
 */
public class LetStmt extends StmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public BindsTopType binds;

    @Override
    public String toString() {
        return "LetStmt{" +
                "binds=" + binds +
                '}';
    }
}
