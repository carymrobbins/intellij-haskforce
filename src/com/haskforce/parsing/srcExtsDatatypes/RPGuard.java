package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RPGuard l (Pat l) [Stmt l]
 */
public class RPGuard extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public StmtTopType[] stmts;

    @Override
    public String toString() {
        return "RPGuard{" +
                "pat=" + pat +
                ", stmts=" + Arrays.toString(stmts) +
                '}';
    }
}
