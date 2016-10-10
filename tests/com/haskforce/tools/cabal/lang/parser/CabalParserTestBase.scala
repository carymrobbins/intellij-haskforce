package com.haskforce.tools.cabal.lang.parser

import com.intellij.openapi.util.io.FileUtil

import com.haskforce.haskell.parser.HaskellParserTestBase

abstract class CabalParserTestBase
  extends HaskellParserTestBase(
    FileUtil.join("tools", "cabal", "parser"),
    "cabal",
    /* lowercaseFirstLetter = */ true,
    new CabalParserDefinition
  )

