package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data QualStmt l
 *  = QualStmt     l (Stmt l)         -- ^ an ordinary statement
 *  | ThenTrans    l (Exp l)          -- ^ @then@ /exp/
 *  | ThenBy       l (Exp l) (Exp l)  -- ^ @then@ /exp/ @by@ /exp/
 *  | GroupBy      l (Exp l)          -- ^ @then@ @group@ @by@ /exp/
 *  | GroupUsing   l (Exp l)          -- ^ @then@ @group@ @using@ /exp/
 *  | GroupByUsing l (Exp l) (Exp l)  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
 */
public class QualStmtTopType {
}
