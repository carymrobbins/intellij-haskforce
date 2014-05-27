package com.haskforce.parsing.srcExtsDatatypes;

/**
 * QualStmt     l (Stmt l)
 */
public class QualStmt extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType stmt;

    @Override
    public String toString() {
        return "QualStmt{" +
                "stmt=" + stmt +
                '}';
    }
}
