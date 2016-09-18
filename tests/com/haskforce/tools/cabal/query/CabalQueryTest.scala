package com.haskforce.tools.cabal.query

import scalaz.syntax.id._


import com.haskforce.tools.cabal.lang.parser.CabalParserTestBase
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.haskforce.test.AssertMixin
import com.haskforce.system.utils.NonEmptySet

class CabalQueryTest extends CabalParserTestBase with AssertMixin {

  def testPackageName(): Unit = {
    blank00001.getPackageName === None
    braces00001.getPackageName === Some("Test1")
    example00001.getPackageName === Some("example-lib")
    example00002.getPackageName === Some("svm")
    example00003.getPackageName === Some("foo-bar")
    example00004.getPackageName === Some("HUnit")
    example00005.getPackageName === Some("feldspar-language")
    if00001.getPackageName === Some("Foo")
    if00002.getPackageName === Some("Test1")
  }

  def testBuildInfoFromSourceFile(): Unit = {
    def findBuildInfo(q: CabalQuery, sourcePath: String) = {
      CabalQuery.findBuildInfoForSourceFile(q.getBuildInfo, "/", sourcePath)
    }
    findBuildInfo(example00001, "/foo.hs").get |> { bi =>
      assertInstanceOf[BuildInfo.Library](bi)
    }
    findBuildInfo(example00001, "/tests/foo.hs").get |> { bi =>
      val ts = assertInstanceOf[BuildInfo.TestSuite](bi)
      ts.getName === Some("test")
    }
    findBuildInfo(example00005, "/foo.hs") === None
    findBuildInfo(example00005, "/src/foo.hs").get |> { bi =>
      assertInstanceOf[BuildInfo.Library](bi)
    }
    findBuildInfo(example00005, "/tests/foo.hs").get |> { bi =>
      val ts = assertInstanceOf[BuildInfo.TestSuite](bi)
      ts.getName === Some("range")
    }
    findBuildInfo(example00005, "/examples/foo.hs").get |> { bi =>
      assertInstanceOf[BuildInfo.Library](bi)
    }
  }

  def testExtensions(): Unit = {
    braces00001.getLibrary.getExtensions === Set(
      "CPP"
    )
    example00001.getTestSuites.head.getExtensions === Set(
      "NoImplicitPrelude",
      "OverloadedStrings",
      "PackageImports",
      "GeneralizedNewtypeDeriving",
      "DeriveGeneric",
      "DeriveFunctor",
      "ImplicitParams",
      "DeriveDataTypeable",
      "ScopedTypeVariables",
      "LambdaCase",
      "FlexibleContexts",
      "FlexibleInstances",
      "MultiParamTypeClasses",
      "RecordWildCards",
      "ViewPatterns",
      "BangPatterns",
      "ConstraintKinds",
      "DataKinds",
      "TypeOperators",
      "NoMonomorphismRestriction",
      "StandaloneDeriving",
      "QuasiQuotes"
    )
  }

  def testDependencies(): Unit = {
    example00005.getLibrary.getDependencies === Set(
      "array",
      "base",
      "containers",
      "data-hash",
      "data-lens",
      "mtl",
      "QuickCheck",
      "patch-combinators",
      "syntactic",
      "tagged",
      "tuple",
      "monad-par",
      "deepseq",
      "random",
      "data-default"
    )
  }

  def testGhcOptions(): Unit = {
    example00001.getLibrary.getGhcOptions === Set(
      "-fcontext-stack=30",
      "-Wall",
      "-fno-warn-missing-signatures",
      "-fno-warn-orphans",
      "-fno-warn-type-defaults",
      "-fno-warn-partial-type-signatures"
    )
    example00001.getTestSuites.head.getGhcOptions === Set(
      "-Wall",
      "-fno-warn-missing-signatures",
      "-fno-warn-orphans",
      "-fno-warn-type-defaults",
      "-threaded",
      "-with-rtsopts=-N",
      "-main-is",
      "ExampleLibTest"
    )
  }

  def testBuildInfoNames(): Unit = {
    example00001.getTestSuites.head.getName === Some("test")
  }

  def testSourceDirs(): Unit = {
    sourceDirs00001.getLibrary.getSourceDirs === NonEmptySet(".")
    sourceDirs00001.getSourceRoots === NonEmptySet(".", "app")
    sourceDirs00001.getTestSourceRoots === Some(NonEmptySet("tests", "bench1", "bench2"))
  }

  lazy val blank00001 = getQuery("blank00001")
  lazy val braces00001 = getQuery("braces00001")
  lazy val example00001 = getQuery("example00001")
  lazy val example00002 = getQuery("example00002")
  lazy val example00003 = getQuery("example00003")
  lazy val example00004 = getQuery("example00004")
  lazy val example00005 = getQuery("example00005")
  lazy val sourceDirs00001 = getQuery("sourceDirs00001")
  lazy val if00001 = getQuery("if00001")
  lazy val if00002 = getQuery("if00002")

  protected def getQuery(name: String) = new CabalQuery(getFile(name))

  protected def getFile(name: String): CabalFile = {
    createPsiFile(name, loadFile(name + "." + myFileExt)) match {
      case f: CabalFile => f
      case other => throw new AssertionError(s"Expected CabalFile but got: $other")
    }
  }
}
