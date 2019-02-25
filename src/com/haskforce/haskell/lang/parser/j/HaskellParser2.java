package com.haskforce.haskell.lang.parser.j;

import com.haskforce.utils.parser.j.PsiParsec;

import static com.haskforce.psi.HaskellTypes.CONID;


public class HaskellParser2 {

  static PsiParsec<Void> pModuleName = PsiParsec.markStart(
    PsiParsec.pif(maybeToken CONID)
  )
}
