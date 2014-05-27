package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Do l [Stmt l]
 */
public class Do extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType[] stmts;

    @Override
    public String toString() {
        return "Do{" +
                "stmts=" + Arrays.toString(stmts) +
                '}';
    }
}
