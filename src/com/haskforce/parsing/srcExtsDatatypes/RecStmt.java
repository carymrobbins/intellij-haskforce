package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RecStmt l [Stmt l]
 */
public class RecStmt extends StmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType[] stmts;

    @Override
    public String toString() {
        return "RecStmt{" +
                "stmts=" + Arrays.toString(stmts) +
                '}';
    }
}
