package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.cabal.query.CabalQuery
import com.haskforce.utils.{PackageYamlUtil, PsiFileParser, YAMLFileUtil}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, ExternalSystemException, ProjectKeys}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import org.jetbrains.yaml.psi.YAMLFile
import prelude._

final class StackProjectResolver extends ExternalSystemProjectResolver[StackExecutionSettings] {

  import StackProjectResolver._

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    new DataNode[ProjectData](
      ProjectKeys.PROJECT,
      new ProjectData(
        StackManager.PROJECT_SYSTEM_ID,
        inferRootProjectName(projectPath)
      )
    )
  }

  private def mkSubProjectNode(dir: ProjectDir): Option[DataNode[ProjectData]] =
    if (!dir.toFile.isDirectory) None else {
      dir.listFiles.map(_.getName == "package.yaml")
    }

  private def inferProjectName(dir: ProjectDir): Option[String] =
    if ()

  private def inferRootProjectName(projectPath: String): Either[ExternalSystemException, String] = {
    val root = new File(projectPath)
    val rootPackageYaml = new File(root, "package.yaml")

    val ePackageYamlName =
      YAMLFileUtil.parseYaml(rootPackageYaml)
        .flatMap(PackageYamlUtil.getName)
        .leftMapNel

    def eRootDirName =
      Either.cond[Throwable, String](
        root.isDirectory,
        root.getName,
        new IllegalArgumentException(s"'${root.getPath}' is not a directory")
      ).leftMapNel

    ePackageYamlName.orElseAccum(eRootDirName).leftMap { xs =>
      new ExternalSystemException(
        "Failed to determine root project name, errors occurred:\n"
          + xs.toList.map(_.getMessage).mkString("\n")
      )
    }
  }

  override def cancelTask(
    taskId: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = ???
}

object StackProjectResolver {

  private val LOG = Logger.getInstance(classOf[StackProjectResolver])

  /**
    * Wraps the standard java File type as a potential project directory.
    * Ensures that the underlying File has already been validated as an
    * existing directory.
    */
  @newtype class ProjectDir(val toFile: File) extends AnyVal {

    /** List only child directories (not files). */
    def listDirs: Array[ProjectDir] =
      toFile.listFiles.iterator.map(ProjectDir.fromFile).collect {
        case Some(dir) => dir
      }.toArray

    /** List only child files (not directories). */
    def listFiles: Array[File] =
      toFile.listFiles.collect { case file if file.isFile => file }
  }

  object ProjectDir {
    def fromFile(file: File): Option[ProjectDir] =
      if (file.isDirectory) Some(file.coerce[ProjectDir]) else None
  }

  sealed trait PackageConfig {
    def packageName: Either[Throwable, String]
  }
  object PackageConfig {
    final case class PackageYaml(psi: YAMLFile) extends PackageConfig {
      override def packageName: Either[Throwable, String] =
        PackageYamlUtil.getName(psi)
    }

    final case class Cabal(psi: CabalFile) extends PackageConfig {

      private val query = new CabalQuery(SPsiFile(psi))

      override def packageName: Either[Throwable, String] =
    }

    def fromFile(file: File): Either[Throwable, Option[PackageConfig]] = {
      file.getName match {
        case "package.yaml" =>
          PsiFileParser.parseForDefaultProject[YAMLFile, File](file)
            .map(x => Some(PackageYaml(x)))
        case name if name.endsWith(".cabal") =>
          PsiFileParser.parseForDefaultProject[CabalFile, File](file)
            .map(x => Some(Cabal(x)))
        case _ =>
          Right(None)
      }
    }
  }
}
