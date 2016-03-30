package com.haskforce.cabal.lang.parser

import com.intellij.openapi.util.io.FileUtil

import com.haskforce.parser.HaskellParserTestBase

class CabalParserTest
  extends HaskellParserTestBase(
    FileUtil.join("cabal", "parser"),
    "cabal",
    /* lowercaseFirstLetter = */ true,
    new CabalParserDefinition
  )
  with CabalParsingTestCases
