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

  def testExtensions(): Unit = {
    assertSameElements(braces00001.getLibrary.get.getExtensions,
      Extension("CPP")
    )
    assertSameElements(example00001.getTestSuites.head.getExtensions,
      Extension("NoImplicitPrelude"),
      Extension("OverloadedStrings"),
      Extension("PackageImports"),
      Extension("GeneralizedNewtypeDeriving"),
      Extension("DeriveGeneric"),
      Extension("DeriveFunctor"),
      Extension("ImplicitParams"),
      Extension("DeriveDataTypeable"),
      Extension("ScopedTypeVariables"),
      Extension("LambdaCase"),
      Extension("FlexibleContexts"),
      Extension("FlexibleInstances"),
      Extension("MultiParamTypeClasses"),
      Extension("RecordWildCards"),
      Extension("ViewPatterns"),
      Extension("BangPatterns"),
      Extension("ConstraintKinds"),
      Extension("DataKinds"),
      Extension("TypeOperators"),
      Extension("NoMonomorphismRestriction"),
      Extension("StandaloneDeriving"),
      Extension("QuasiQuotes")
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
