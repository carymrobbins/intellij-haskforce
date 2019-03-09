package com.haskforce.haskell.lang.lexer;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.FlexLexer;

public class HaskellParsingLexer2 extends FlexAdapter {
  public HaskellParsingLexer2() {
    super(new _HaskellParsingLexer2());
  }

  @Override
  public _HaskellParsingLexer2 getFlex() {
    return (_HaskellParsingLexer2) super.getFlex();
  }
}
