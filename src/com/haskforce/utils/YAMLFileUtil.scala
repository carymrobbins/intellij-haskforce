package com.haskforce.utils

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.intellij.openapi.project.ProjectManager
import com.intellij.psi.PsiFileFactory
import org.jetbrains.yaml.YAMLLanguage
import org.jetbrains.yaml.psi.YAMLFile
import prelude._

object YAMLFileUtil {

  def parseYamlIfExists(file: File): Either[Throwable, Option[YAMLFile]] =
    if (file.isFile) parseYaml(file).map(Some(_)) else Right(None)

  def parseYaml(file: File): Either[Throwable, YAMLFile] =
    Either.catchNonFatal(file.toPath).flatMap(parseYaml)

  def parseYaml(path: Path): Either[Throwable, YAMLFile] =
    Either.catchNonFatal(
      PsiFileFactory
        .getInstance(ProjectManager.getInstance.getDefaultProject)
        .createFileFromText(
          YAMLLanguage.INSTANCE,
          new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        )
    ).flatMap {
      case x: YAMLFile => Right(x)
      case x => Left(new IllegalStateException(s"Unexpected parsed yaml file as ${x.getClass} '$path'"))
    }
}
