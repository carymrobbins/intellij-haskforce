package com.haskforce.utils

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.haskforce.cabal.CabalLanguage
import com.haskforce.cabal.lang.psi.CabalFile
import com.intellij.lang.Language
import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.psi.{PsiFile, PsiFileFactory}
import org.jetbrains.yaml.YAMLLanguage
import org.jetbrains.yaml.psi.YAMLFile
import prelude._

import scala.reflect.ClassTag

trait PsiFileParser[F <: PsiFile] {
  def parse(factory: PsiFileFactory, input: String): Either[Throwable, F]
}

object PsiFileParser {

  def apply[F <: PsiFile](implicit ev: PsiFileParser[F]): PsiFileParser[F] = ev

  def parse[F <: PsiFile : PsiFileParser, I : Parseable](
    factory: PsiFileFactory, input: I
  ): Either[Throwable, F] =
    Parseable[I].getInput(input).flatMap(apply[F].parse(factory, _))

  def parseForProject[F <: PsiFile : PsiFileParser, I : Parseable](
    project: Project, input: I
  ): Either[Throwable, F] =
    Parseable[I].getInput(input).flatMap(
      apply[F].parse(PsiFileFactory.getInstance(project), _)
    )

  def parseForDefaultProject[F <: PsiFile : PsiFileParser, I : Parseable](
    input: I
  ): Either[Throwable, F] =
    Parseable[I].getInput(input).flatMap(
      apply[F].parse(
        PsiFileFactory.getInstance(ProjectManager.getInstance.getDefaultProject),
        _
      )
    )

  implicit val yaml: PsiFileParser[YAMLFile] = fromLanguage(YAMLLanguage.INSTANCE)
  implicit val cabal: PsiFileParser[CabalFile] = fromLanguage(CabalLanguage.INSTANCE)

  def fromLanguage[F <: PsiFile](lang: Language)(implicit ct: ClassTag[F]): PsiFileParser[F] =
    (factory: PsiFileFactory, input: String) =>
      Either.catchNonFatal {
        factory.createFileFromText(lang, input)
      }.flatMap {
        case ct(psiFile) => Right(psiFile)
        case x =>
          Left(new IllegalStateException(
            s"PsiFileParser expected ${ct.runtimeClass} but got ${x.getClass}"
          ))
      }

  trait Parseable[A] {
    def getInput(a: A): Either[Throwable, String]
  }

  object Parseable {

    def apply[A](implicit ev: Parseable[A]): Parseable[A] = ev

    implicit val string: Parseable[String] = Right(_)

    implicit val path: Parseable[Path] =
      path => Either.catchNonFatal(
        new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      )

    implicit val file: Parseable[File] =
      file => Either.catchNonFatal(file.toPath).flatMap(path.getInput)
  }
}
