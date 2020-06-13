package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.utils.PsiFileParser
import org.jetbrains.yaml.psi.YAMLFile

final case class PackageConfig(
  name: String
)

object PackageConfig {

  def fromPackageYaml(packageYaml: YAMLFile): Either[Throwable, PackageConfig] = {
    for {
      name <- PackageYamlQuery.getName(packageYaml)
    } yield PackageConfig(
      name
    )
  }

  def fromCabalFile(cabalFile: CabalFile): Either[Throwable, PackageConfig] = {
    val name = cabalFile.getName.split('.').head
    Right(
      PackageConfig(
        name
      )
    )
  }

  def fromFile(file: File): Either[Throwable, Option[PackageConfig]] = {
    file.getName match {
      case "package.yaml" =>
        PsiFileParser.parseForDefaultProject[YAMLFile, File](file)
          .flatMap(fromPackageYaml)
          .map(Option.apply)
      case name if name.endsWith(".cabal") =>
        PsiFileParser.parseForDefaultProject[CabalFile, File](file)
          .flatMap(fromCabalFile)
          .map(Option.apply)
      case _ =>
        Right(None)
    }
  }
}
