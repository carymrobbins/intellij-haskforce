package com.haskforce.cabal.lang.psi.impl

import com.intellij.psi.PsiElement

class CabalElementTypeError(expected: String, got: PsiElement)
  extends AssertionError(
    s"Expected $expected but got $got (${got.getText})"
  )
