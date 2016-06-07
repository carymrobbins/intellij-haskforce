package com.haskforce.cabal.query

import scalaz.syntax.id._

import com.intellij.testFramework.UsefulTestCase._
import junit.framework.TestCase._

import com.haskforce.cabal.lang.parser.CabalParserTestBase
import com.haskforce.cabal.lang.psi.CabalFile

class CabalQueryTest extends CabalParserTestBase {

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
      assertInstanceOf(bi, classOf[BuildInfo.Library])
    }
    findBuildInfo(example00001, "/tests/foo.hs").get |> { bi =>
      val ts = assertInstanceOf(bi, classOf[BuildInfo.TestSuite])
      ts.getName === Some("test")
    }
    findBuildInfo(example00005, "/foo.hs") === None
    findBuildInfo(example00005, "/src/foo.hs").get |> { bi =>
      assertInstanceOf(bi, classOf[BuildInfo.Library])
    }
    findBuildInfo(example00005, "/tests/foo.hs").get |> { bi =>
      val ts = assertInstanceOf(bi, classOf[BuildInfo.TestSuite])
      ts.getName === Some("range")
    }
    findBuildInfo(example00005, "/examples/foo.hs").get |> { bi =>
      assertInstanceOf(bi, classOf[BuildInfo.Library])
    }
  }

  def testExtensions(): Unit = {
    assertSameElements(braces00001.getLibrary.get.getExtensions,
      "CPP"
    )
    assertSameElements(example00001.getTestSuites.head.getExtensions,
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
    assertSameElements(example00005.getLibrary.get.getDependencies,
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
    assertSameElements(example00001.getLibrary.get.getGhcOptions,
      "-fcontext-stack=30",
      "-Wall",
      "-fno-warn-missing-signatures",
      "-fno-warn-orphans",
      "-fno-warn-type-defaults",
      "-fno-warn-partial-type-signatures"
    )
    assertSameElements(example00001.getTestSuites.head.getGhcOptions,
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

  lazy val blank00001 = getQuery("blank00001")
  lazy val braces00001 = getQuery("braces00001")
  lazy val example00001 = getQuery("example00001")
  lazy val example00002 = getQuery("example00002")
  lazy val example00003 = getQuery("example00003")
  lazy val example00004 = getQuery("example00004")
  lazy val example00005 = getQuery("example00005")
  lazy val if00001 = getQuery("if00001")
  lazy val if00002 = getQuery("if00002")

  protected def getQuery(name: String) = new CabalQuery(getFile(name))

  protected def getFile(name: String): CabalFile = {
    createPsiFile(name, loadFile(name + "." + myFileExt)) match {
      case f: CabalFile => f
      case other => throw new AssertionError(s"Expected CabalFile but got: $other")
    }
  }

  implicit final class RichAssertions[A](val underlying: A) {
    def === [B](other: B): Unit = assertEquals(other, underlying)
  }

  protected def assertEmptyArray(array: Array[_]): Unit = {
    assertEmpty(array.asInstanceOf[Array[AnyRef]])
  }
}
