package com.haskforce.cabal.query

import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.tree.IElementType

import com.haskforce.cabal.lang.psi
import com.haskforce.cabal.lang.psi.CabalTypes
import com.haskforce.utils.PQ

final class CabalQuery(val psiFile: psi.CabalFile) {

  lazy val getVirtualFile = Option(psiFile.getVirtualFile)

  lazy val getFilePath = getVirtualFile.flatMap(f => Option(f.getCanonicalPath))

  lazy val getDirPath = for {
    f <- getVirtualFile
    d <- Option(f.getParent)
    p <- Option(d.getCanonicalPath)
  } yield p

  def getPackageName: Option[String] = for {
    pkgName <- PQ.getChildOfType(psiFile, classOf[psi.PkgName])
    ff <- PQ.getChildOfType(pkgName, classOf[psi.Freeform])
  } yield ff.getText

  def getLibrary: Option[BuildInfo.Library] = {
    psiFile.getChildren.collectFirst {
      case c: psi.Library => new BuildInfo.Library(c)
    }
  }

  def getExecutables: Array[BuildInfo.Executable] = {
    psiFile.getChildren.collect {
      case c: psi.Executable => new BuildInfo.Executable(c)
    }
  }

  def getTestSuites: Array[BuildInfo.TestSuite] = {
    psiFile.getChildren.collect {
      case c: psi.TestSuite => new BuildInfo.TestSuite(c)
    }
  }

  def getBenchmarks: Array[BuildInfo.Benchmark] = {
    psiFile.getChildren.collect {
      case c: psi.Benchmark => new BuildInfo.Benchmark(c)
    }
  }

  def getBuildInfo: Array[BuildInfo] = {
    psiFile.getChildren.collect {
      case c: psi.Library => new BuildInfo.Library(c)
      case c: psi.Executable => new BuildInfo.Executable(c)
      case c: psi.TestSuite => new BuildInfo.TestSuite(c)
      case c: psi.Benchmark => new BuildInfo.Benchmark(c)
    }
  }

  def findBuildInfoForSourceFile(sourceFilePath: String): Option[BuildInfo] = {
    getDirPath.flatMap(baseDir =>
      CabalQuery.findBuildInfoForSourceFile(getBuildInfo, baseDir, sourceFilePath)
    )
  }

  def findBuildInfoForSourceFile(psiFile: PsiFile): Option[BuildInfo] = {
    Option(psiFile.getVirtualFile).flatMap(findBuildInfoForSourceFile)
  }

  def findBuildInfoForSourceFile(vFile: VirtualFile): Option[BuildInfo] = {
    Option(vFile.getCanonicalPath).flatMap(findBuildInfoForSourceFile)
  }
}

object CabalQuery {

  /**
   * Returns the first BuildInfo which has the most specific source dir and
   * contains the specified 'sourcePath' given that 'baseDir' is the root
   * directory of the source dirs.  As such, if two different BuildInfo source dirs
   * could be an ancestor of 'sourcePath', then the first BuildInfo listed
   * with the longest matching source dir wins.
   *
   * Note that this method is provided specifically for testing without the need
   * of having VirtualFile instances which correspond to real files.  In real code,
   * prefer the instance method provided in the 'CabalQuery' class.
   */
  def findBuildInfoForSourceFile
      (infos: Array[BuildInfo],
       baseDir: String,
       sourcePath: String)
      : Option[BuildInfo] = {
    if (!sourcePath.startsWith(baseDir)) return None
    var result: Option[(String, BuildInfo)] = None
    infos.toStream.foreach { info =>
      info.getSourceDirs.foreach { sourceDir =>
        if (FileUtil.isAncestor(FileUtil.join(baseDir, sourceDir), sourcePath, true)) {
          if (!result.exists(_._1.length >= sourceDir.length)) {
            result = Some((sourceDir, info))
          }
        }
      }
    }
    result.map(_._2)
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

sealed trait BuildInfo extends ElementWrapper  {

  val typ: BuildInfo.Type

  val el: PsiElement

  /** Returns all listed extensions. */
  def getExtensions: Array[String] = {
    el.getChildren.collect {
      case c: psi.Extensions => c
      case c: psi.DefaultExtensions => c
      case c: psi.OtherExtensions => c
    }.flatMap(c =>
      PQ.getChildOfType(c, classOf[psi.IdentList])
    ).flatMap(c =>
      PQ.getChildNodes(c, CabalTypes.IDENT).map(_.getText)
    )
  }

  /** Returns the listed dependencies' package names. */
  def getDependencies: Array[String] = {
    el.getChildren.collectFirst {
      case c: psi.BuildDepends => c.getPackageNames
    }.getOrElse(Array.empty)
  }

  def getGhcOptions: Array[String] = {
    el.getChildren.collect {
      case c: psi.impl.GhcOptionsImpl => c.getValue
    }.flatten
  }

  /** Get hs-source-dirs listed, defaulting to "." if not present. */
  def getSourceDirs: Array[String] = {
    el.getChildren.collectFirst {
      case c: psi.HsSourceDirs => c.getValue
    }.getOrElse(Array("."))
  }
}

object BuildInfo {

  sealed trait Type
  object Type {
    case object Library extends Type
    case object Executable extends Type
    case object TestSuite extends Type
    case object Benchmark extends Type
  }

  val LIBRARY_TYPE_NAME = "library"
  final class Library(val el: psi.Library) extends BuildInfo {
    override val typ = Type.Library
  }

  val EXECUTABLE_TYPE_NAME = "executable"
  final class Executable(val el: psi.Executable) extends BuildInfo with Named {
    override val typ = Type.Executable
    override val NAME_ELEMENT_TYPE = CabalTypes.EXECUTABLE_NAME
  }

  val TEST_SUITE_TYPE_NAME = "test-suite"
  final class TestSuite(val el: psi.TestSuite) extends BuildInfo with Named {
    override val typ = Type.TestSuite
    override val NAME_ELEMENT_TYPE = CabalTypes.TEST_SUITE_NAME
  }

  final class Benchmark(val el: psi.Benchmark) extends BuildInfo with Named {
    override val typ = Type.Benchmark
    override val NAME_ELEMENT_TYPE = CabalTypes.BENCHMARK_NAME
  }
}
