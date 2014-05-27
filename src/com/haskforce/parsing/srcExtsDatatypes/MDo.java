package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * MDo l [Stmt l]
 */
public class MDo extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType[] stmts;

    @Override
    public String toString() {
        return "MDo{" +
                "stmts=" + Arrays.toString(stmts) +
                '}';
    }
}
