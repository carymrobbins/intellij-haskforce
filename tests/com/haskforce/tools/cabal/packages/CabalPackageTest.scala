package com.haskforce.tools.cabal.packages

import com.haskforce.system.packages.BuildType.{Library, TestSuite}
import com.haskforce.system.packages.{BuildType, FileError, HPackage}
import com.haskforce.test.AssertMixin
import com.haskforce.tools.cabal.lang.parser.CabalParserTestBase
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.intellij.mock.MockProjectEx
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}

class CabalPackageTest extends CabalParserTestBase with AssertMixin {

  def testPackageName(): Unit = {
    blank00001.getName === None
    braces00001.getName === Some("Test1")
    example00001.getName === Some("example-lib")
    example00002.getName === Some("svm")
    example00003.getName === Some("foo-bar")
    example00004.getName === Some("HUnit")
    example00005.getName === Some("feldspar-language")
    if00001.getName === Some("Foo")
    if00002.getName === Some("Test1")
  }

  def testBuildInfoFromSourceFile(): Unit = {
    def assertBuildInfo(hPackage: HPackage, string: String, buildType: BuildType) = {
      hPackage.getBestMatchingBuildInfo(string).typ == buildType
    }

    assertBuildInfo(example00001, "/foo.hs", Library)

    assertBuildInfo(example00001, "/tests/foo.hs", TestSuite)

    example00005.getBestMatchingBuildInfo("/tests/foo.hs") == example00005.getLibrary

    assertBuildInfo(example00005, "/src/foo.hs", Library)

    assertBuildInfo(example00005, "/tests/foo.hs", TestSuite)

    assertBuildInfo(example00005, "/examples/foo.hs", Library)
  }

  def testExtensions(): Unit = {
    braces00001.getLibrary.getExtensions === Set(
      "CPP"
    )
    example00001.getTests.head.getExtensions === Set(
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
    example00001.getTests.head.getGhcOptions === Set(
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

  def testSourceDirs(): Unit = {
    sourceDirs00001.getLibrary.getSourceDirs === Set(".")
    sourceDirs00001.getSources.flatMap(_.getSourceDirs) === Set(".", "app")
    sourceDirs00001.getTests.flatMap(_.getSourceDirs) === Set("tests", "bench1", "bench2")
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

  protected def getQuery(name: String) = {
    this.setUp()
    val project: MockProjectEx = getProject
    project.init()
    val fullName = name + "." + myFileExt
    val fullPath = "tests/gold/cabal/parser/" + fullName
    val path: VirtualFile = LocalFileSystem.getInstance().findFileByPath(fullPath)
    if (path == null) throw new AssertionError(s"VirtualFile for file is null")
    new CabalPackage(getFile(fullName), path)
  }

  protected def getFile(name: String): CabalFile = {
    createPsiFile(name, loadFile(name)) match {
      case f: CabalFile => f
      case other => throw new AssertionError(s"Expected CabalFile but got: $other")
    }
  }
}

