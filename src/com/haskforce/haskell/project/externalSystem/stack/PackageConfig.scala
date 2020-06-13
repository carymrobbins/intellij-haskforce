package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.utils.PsiFileParser

final case class PackageConfig(
  name: String
)

object PackageConfig {

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
      case name if name.endsWith(".cabal") =>
        PsiFileParser.parseForDefaultProject[CabalFile, File](file)
          .flatMap(fromCabalFile)
          .map(Option.apply)
      case _ =>
        Right(None)
    }
  }
}
