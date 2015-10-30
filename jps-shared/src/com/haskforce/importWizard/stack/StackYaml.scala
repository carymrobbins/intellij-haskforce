package com.haskforce.importWizard.stack

import java.io.File
import java.util

import scala.collection.JavaConversions._
import scalaz.\/
import scalaz.syntax.either._

import com.intellij.openapi.util.io.FileUtil
import org.yaml.snakeyaml.Yaml

/**
 * The parse result of a stack.yaml file.
 */
sealed case class StackYaml(packages: util.List[StackYaml.Package])

object StackYaml {
  sealed case class Package(path: String)

  def fromFile(path: String): String \/ StackYaml = {
    readFile(path).flatMap(fromString)
  }

  def unsafeFromFile(path: String): StackYaml = fromFile(path).valueOr(
    e => throw new RuntimeException(s"Could not parse stack yaml '$path': $e")
  )

  private def readFile(path: String): String \/ String = {
    \/.fromTryCatchNonFatal {
      val source = scala.io.Source.fromFile(path)
      try source.mkString finally source.close()
    }.leftMap(_.getMessage)
  }

  def fromString(doc: String): String \/ StackYaml = for {
    yaml <- rawFromString(doc)
    mbPackages = Option(yaml.get("packages"))
    packagesRaw <- \/.fromEither(mbPackages.toRight("Expected key 'packages'"))
    packages <- safeCast[java.util.List[String]]("Expected 'packages' to be a list")(packagesRaw)
  } yield StackYaml(packages.map(Package))

  def rawFromString(doc: String): String \/ util.LinkedHashMap[String, Object] = {
    \/.fromTryCatchNonFatal(new Yaml().load(doc)).leftMap(_.getMessage).flatMap(
      safeCast[util.LinkedHashMap[String, Object]]("Unable to parse yaml file")
    )
  }

  def safeCast[A : Manifest](err: String)(o: AnyRef): String \/ A = {
    val cls = manifest[A].runtimeClass
    if (cls.isInstance(o)) o.asInstanceOf[A].right
    else err.left
  }
}

object StackYamlUtil {
  def findCabalFile(projectRoot: String, pkg: StackYaml.Package): Option[File] = {
    val dir = new File(FileUtil.join(projectRoot, pkg.path))
    Option(dir.listFiles()).flatMap(_.collectFirst {
      case file if file.getName.endsWith(".cabal") => file
    })
  }

  def unsafeFindCabalFile(projectRoot: String, pkg: StackYaml.Package): File = {
    findCabalFile(projectRoot, pkg).getOrElse(
      throw new RuntimeException(s"Could not find cabal file for package '${FileUtil.join(projectRoot, pkg.path)}'")
    )
  }
}
