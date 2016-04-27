package com.haskforce.cabal.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class CabalParser2 implements PsiParser {
  @NotNull
  @Override
  public ASTNode parse(@NotNull IElementType root, @NotNull PsiBuilder builder) {
    System.out.println("------ PARSER START -------");
    PsiBuilder.Marker m = builder.mark();
    CabalYaccParser yacc = new CabalYaccParser(builder);
    //yacc.yydebug = true;
    yacc.yyparse();
    if (!builder.eof()) {
      PsiBuilder.Marker err = builder.mark();
      while (!builder.eof()) builder.advanceLexer();
      err.error("Expected end of file");
    }
    m.done(root);
    return builder.getTreeBuilt();
  }
}
