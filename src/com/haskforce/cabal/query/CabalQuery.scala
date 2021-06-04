package com.haskforce.cabal.query

import prelude._

import java.io.{File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.haskforce.cabal.CabalLanguage
import com.haskforce.cabal.lang.psi
import com.haskforce.cabal.lang.psi.{CabalFile, CabalTypes}
import com.haskforce.utils.{IJReadAction, NonEmptySet}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.openapi.util.io.FileUtil
import com.intellij.psi.tree.IElementType
import com.intellij.psi.{PsiElement, PsiFileFactory}

final class CabalQuery(val cabalFile: SPsiFile[CabalFile]) {

  lazy val getVirtualFile: Option[SVirtualFile] = cabalFile.getVirtualFile

  lazy val getFilePath: Option[String] = getVirtualFile.flatMap(_.getCanonicalPath)

  lazy val getDirPath = for {
    f <- getVirtualFile
    d <- f.getParent
    p <- d.getCanonicalPath
  } yield p

  def getPackageName: IJReadAction[Option[String]] = (for {
    pkgName <- OptionT(cabalFile.getChildOfType[psi.PkgName])
    ff <- OptionT(pkgName.getChildOfType[psi.Freeform])
    text <- ff.getText.liftM[OptionT]
  } yield text).run

  def getLibrary: IJReadAction[Option[BuildInfo.Library]] = {
    cabalFile.getChildOfType[psi.Library].map { maybeEl =>
      maybeEl.map(el =>
        new BuildInfo.Library(el)
      )
    }
  }

  def getExecutables: IJReadAction[Vector[BuildInfo.Executable]] = {
    cabalFile.getChildrenOfType[psi.Executable].map(_.map(new BuildInfo.Executable(_)))
  }

  def getTestSuites: IJReadAction[Vector[BuildInfo.TestSuite]] = {
    cabalFile.getChildrenOfType[psi.TestSuite].map(_.map(new BuildInfo.TestSuite(_)))
  }

  def getBenchmarks: IJReadAction[Vector[BuildInfo.Benchmark]] = {
    cabalFile.getChildrenOfType[psi.Benchmark].map(_.map(new BuildInfo.Benchmark(_)))
  }

  def getBuildInfo: IJReadAction[Array[BuildInfo]] = getLibrary.map { maybeLibrary =>
    // Since we're in a .map, the .getChildren access is safe.
    val others = cabalFile.toPsiFile.getChildren.collect {
      case c: psi.Executable => new BuildInfo.Executable(SPsiElement(c))
      case c: psi.TestSuite => new BuildInfo.TestSuite(SPsiElement(c))
      case c: psi.Benchmark => new BuildInfo.Benchmark(SPsiElement(c))
    }
    maybeLibrary.toArray ++ others
  }

  def findBuildInfoForFilePath(sourceFilePath: String): IJReadAction[Option[BuildInfo]] = {
    for {
      baseDir <- OptionT(IJReadAction(getDirPath))
      infos <- getBuildInfo.liftM[OptionT]
      info <- OptionT(CabalQuery.findBuildInfoForSourceFile(infos, baseDir, sourceFilePath))
    } yield info
  }.run

  def findBuildInfoForPsiFile(psiFile: SPsiFile.Top): IJReadAction[Option[BuildInfo]] = {
    psiFile.getVirtualFile.cata(
      findBuildInfoForVirtualFile,
      IJReadAction(None)
    )
  }

  def findBuildInfoForVirtualFile(vFile: SVirtualFile): IJReadAction[Option[BuildInfo]] = {
    vFile.getCanonicalPath.cata(
      findBuildInfoForFilePath,
      IJReadAction(None)
    )
  }

  /** Determined from 'library' and 'executable' or root stanza; defaults to "." */
  def getSourceRoots: IJReadAction[NonEmptySet[String]] = for {
    maybeLib <- getLibrary
    maybeLibSourceDirs <- maybeLib.traverse(_.getSourceDirs)
    libSourceDirs = maybeLibSourceDirs.map(_.toSet).getOrElse(Set.empty)
    exes <- getExecutables
    exeSourceDirSets <- exes.traverse(_.getSourceDirs)
    exeSourceDirs = exeSourceDirSets.iterator.flatMap(_.iterator).toSet
    allSourceDirs = libSourceDirs ++ exeSourceDirs
    res = NonEmptySet.fromSet(allSourceDirs).getOrElse(NonEmptySet("."))
  } yield res

  /** Determined from 'test-suite' or 'benchmark' stanzas, if any exist. */
  def getTestSourceRoots: IJReadAction[Option[NonEmptySet[String]]] = for {
    ts <- getTestSuites
    bs <- getBenchmarks
    roots <- (ts ++ bs).traverse(_.getSourceDirs)
  } yield NonEmptySet.fromNonEmptySets(roots)
}

object CabalQuery {

  private val LOG = Logger.getInstance(classOf[CabalQuery])

  def fromJavaFile(optProject: Option[Project], file: File): Option[CabalQuery] = {
    val project = optProject.getOrElse(ProjectManager.getInstance().getDefaultProject)
    val text = try {
      new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
    } catch {
      case e: IOException =>
        LOG.warn(s"Could not read CabalFile $file: $e", e)
        return None
    }
    PsiFileFactory.getInstance(project).createFileFromText(
      file.getName, CabalLanguage.INSTANCE, text
    ) match {
      case psiFile: CabalFile => Some(new CabalQuery(SPsiFile(psiFile)))
      case other =>
        LOG.warn(new AssertionError(s"Expected CabalFile, got: ${other.getClass}"))
        None
    }
  }

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
  def findBuildInfoForSourceFile(
    infos: Array[BuildInfo],
    baseDir: String,
    sourcePath: String
  ): IJReadAction[Option[BuildInfo]] = {
    if (!sourcePath.startsWith(baseDir)) {
      IJReadAction(None)
    } else {
      infos.toVector.traverse(
        info => info.getSourceDirs.map(_.iterator.map((_, info)).toVector)
      ).map(xss => lookupMostSpecificBuildInfo(baseDir, sourcePath, xss.flatten))
    }
  }

  private def lookupMostSpecificBuildInfo(
    baseDir: String,
    sourcePath: String,
    xs: Vector[(String, BuildInfo)]
  ): Option[BuildInfo] = {
    xs.foldLeft(Option.empty[(String, BuildInfo)]) { case (acc, t@(sourceDir, _)) =>
      if (
        FileUtil.isAncestor(FileUtil.join(baseDir, sourceDir), sourcePath, true)
          && !acc.exists(_._1.length >= sourceDir.length)
      ) Some(t) else {
        acc
      }
    }.map(_._2)
  }
val defaultSourceRoots = NonEmptySet(".") }
trait ElementWrapper {
  type PsiType <: PsiElement
  val el: SPsiElement[PsiType]
}

sealed trait Named extends ElementWrapper {

  val NAME_ELEMENT_TYPE: IElementType

  def getName: IJReadAction[Option[String]]
    = el.getChildNodes(NAME_ELEMENT_TYPE).map(_.headOption.map(_.getText))
}

sealed trait ExecLike extends ElementWrapper {

  def getMainIs: IJReadAction[Option[String]] = {
    for {
      mainIs <- OptionT(el.getChildOfType[psi.MainIs])
      freeform <- OptionT(mainIs.getChildOfType[psi.Freeform])
      text <- freeform.getText.liftM[OptionT]
    } yield text
  }.run
}


sealed trait BuildInfo extends ElementWrapper  {

  val typ: BuildInfo.Type

  /** Returns all listed extensions. */
  def getExtensions: IJReadAction[Set[String]] = for {
    exts <- el.getChildrenOfType[psi.impl.ExtensionsImpl]
    idents <- exts.traverse(_.getChildOfType[psi.IdentList]).map(_.flatten)
    nodes <- idents.traverse(_.getChildNodes(CabalTypes.IDENT)).map(_.flatten)
  } yield nodes.iterator.map(_.getText).toSet

  /** Returns the listed dependencies' package names. */
  def getDependencies: IJReadAction[Set[String]] = {
    el.getChildOfType[psi.BuildDepends].map {
      case Some(e) => e.toPsiElement.getPackageNames.toSet
      case None => Set.empty
    }
  }

  def getGhcOptions: IJReadAction[Set[String]] = {
    el.getChildrenOfType[psi.impl.GhcOptionsImpl].map(children =>
      children.flatMap(_.toPsiElement.getValue).toSet
    )
  }

  /** Get hs-source-dirs listed, defaulting to "." if not present. */
  def getSourceDirs: IJReadAction[NonEmptySet[String]] = {
    el.getChildrenOfType[psi.impl.SourceDirsImpl].map(children =>
      NonEmptySet.fromSets[String](
        children.map(_.toPsiElement.getValue.toSet)
      ).getOrElse(CabalQuery.defaultSourceRoots)
    )
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
  final class Library(val el: SPsiElement[psi.Library]) extends BuildInfo {
    override type PsiType = psi.Library
    override val typ = Type.Library
  }

  val EXECUTABLE_TYPE_NAME = "executable"
  final class Executable(val el: SPsiElement[psi.Executable])
    extends BuildInfo with Named with ExecLike {

    override type PsiType = psi.Executable
    override val typ = Type.Executable
    override val NAME_ELEMENT_TYPE = CabalTypes.EXECUTABLE_NAME
  }

  val TEST_SUITE_TYPE_NAME = "test-suite"
  final class TestSuite(val el: SPsiElement[psi.TestSuite])
    extends BuildInfo with Named with ExecLike {

    override type PsiType = psi.TestSuite
    override val typ = Type.TestSuite
    override val NAME_ELEMENT_TYPE = CabalTypes.TEST_SUITE_NAME
  }

  final class Benchmark(val el: SPsiElement[psi.Benchmark])
    extends BuildInfo with Named with ExecLike {

    override type PsiType = psi.Benchmark
    override val typ = Type.Benchmark
    override val NAME_ELEMENT_TYPE = CabalTypes.BENCHMARK_NAME
  }
}
