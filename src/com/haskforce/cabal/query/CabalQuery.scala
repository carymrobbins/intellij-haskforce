package com.haskforce.cabal.query

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.util.PsiTreeUtil

import com.haskforce.cabal.lang.psi
import com.haskforce.cabal.lang.psi.CabalTypes

final class CabalQuery(file: psi.CabalFile) {

  def getPackageName: Option[String] = for {
    pkgName <- PQ.getChildOfType(file, classOf[psi.PkgName])
    ff <- PQ.getChildOfType(pkgName, classOf[psi.Freeform])
  } yield ff.getText

  def getLibrary: Option[BuildInfo.Library] = {
    file.getChildren.collectFirst {
      case c: psi.Library => new BuildInfo.Library(c)
    }
  }

  def getExecutables: Array[BuildInfo.Executable] = {
    file.getChildren.collect {
      case c: psi.Executable => new BuildInfo.Executable(c)
    }
  }

  def getTestSuites: Array[BuildInfo.TestSuite] = {
    file.getChildren.collect {
      case c: psi.TestSuite => new BuildInfo.TestSuite(c)
    }
  }

  def getBenchmarks: Array[BuildInfo.Benchmark] = {
    file.getChildren.collect {
      case c: psi.Benchmark => new BuildInfo.Benchmark(c)
    }
  }

  def getBuildInfo: Array[BuildInfo] = {
    file.getChildren.collect {
      case c: psi.Library => new BuildInfo.Library(c)
      case c: psi.Executable => new BuildInfo.Executable(c)
      case c: psi.TestSuite => new BuildInfo.TestSuite(c)
      case c: psi.Benchmark => new BuildInfo.Benchmark(c)
    }
  }
}

/** Safer version of PsiTreeUtil which wraps nullable results in Option. */
object PQ {

  def getChildOfType[T <: PsiElement](el: PsiElement, cls: Class[T]): Option[T] = {
    Option(PsiTreeUtil.getChildOfType(el, cls))
  }

  def getChildNodes(el: PsiElement, typ: IElementType, typs: IElementType*): Array[ASTNode] = {
    el.getNode.getChildren(TokenSet.create(typ +: typs: _*))
  }
}

trait ElementWrapper {
  val el: PsiElement
}

sealed trait Named extends ElementWrapper {

  val NAME_ELEMENT_TYPE: IElementType

  def getName: Option[String] = {
    PQ.getChildNodes(el, NAME_ELEMENT_TYPE).headOption.map(_.getText)
  }
}

trait BuildInfo extends ElementWrapper {

  val el: PsiElement

  def getExtensions: Array[Extension] = {
    el.getChildren.collect {
      case c: psi.Extensions => c
      case c: psi.DefaultExtensions => c
      case c: psi.OtherExtensions => c
    }.flatMap(c =>
      PQ.getChildOfType(c, classOf[psi.IdentList])
    ).flatMap(c =>
      PQ.getChildNodes(c, CabalTypes.IDENT).map(n => Extension(n.getText))
    )
  }
}

object BuildInfo {

  final class Library(val el: psi.Library) extends BuildInfo

  final class Executable(val el: psi.Executable) extends BuildInfo with Named {
    override val NAME_ELEMENT_TYPE = CabalTypes.EXECUTABLE_NAME
  }

  final class TestSuite(val el: psi.TestSuite) extends BuildInfo with Named {
    override val NAME_ELEMENT_TYPE = CabalTypes.TEST_SUITE_NAME
  }

  final class Benchmark(val el: psi.Benchmark) extends BuildInfo with Named {
    override val NAME_ELEMENT_TYPE = CabalTypes.BENCHMARK_NAME
  }
}

final class Dependency(el: psi.Dependency)

final case class Extension(name: String)
