package com.haskforce.importWizard.stack

import java.io.File
import java.util

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.traverse._
import scalaz.std.list._

import com.intellij.openapi.util.io.FileUtil

/**
 * The parse result of a stack.yaml file.
 *
 * TODO: Remove in favor of a PSI interface.
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
    assoc <- Yaml.parse(doc).flatMap(_.assoc).leftMap(_.message)
    packages <- parsePackages(assoc)
  } yield StackYaml(packages.map(Package(_)).asJava)

  private def parsePackages(assoc: Map[String, Yaml]): String \/ List[String] = {
    assoc.get("packages") match {
      // Stack defaults to using the root dir if there is no packages field.
      case None => List(".").right
      // Any non-string values in the packages list are omitted (e.g. "location" values).
      // See https://github.com/carymrobbins/intellij-haskforce/issues/263
      case Some(y) => y.list.map(pkgs => pkgs.flatMap(_.string.toOption)).leftMap(_.message)
    }
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


/** A wrapper around snakeyaml to provide a safer interface. */
object Yaml {

  final case class Error(message: String)

  def parse(doc: String): Error \/ Yaml = {
    try {
      fromObject(new org.yaml.snakeyaml.Yaml().load(doc))
    } catch {
      case NonFatal(e) => Error(e.getMessage).left
    }
  }

  def fromObject(o: Any): Error \/ Yaml = o match {
    case m: util.Map[_, _] =>
      m.asScala.map { case (k, v) =>
        k match {
          case kk: String => fromObject(v).map(vv => (kk, vv))
          case _ => Error(s"Key in map must be String, got: $k").left
        }
      }.toList.sequenceU.map(kvs => YamlAssoc(kvs.toMap))

    case xs: util.List[_] =>
      xs.asScala.map(fromObject).toList.sequenceU.map(YamlList(_))

    case s: String => YamlString(s).right
    case n: Int => YamlInt(n).right
    case d: Double => YamlDouble(d).right
    case b: Boolean => YamlBoolean(b).right

    case other =>
      val cls = Option(other).map(_.getClass.getName).getOrElse("")
      Error(s"Unsupported Yaml type $cls: $other").left
  }
}

sealed trait Yaml {

  def assoc: Yaml.Error \/ Map[String, Yaml] = this match {
    case YamlAssoc(m) => m.right
    case _ => Yaml.Error(s"Expected YamlAssoc, got: $this").left
  }

  def list: Yaml.Error \/ List[Yaml] = this match {
    case YamlList(xs) => xs.right
    case _ => Yaml.Error(s"Expected YamlList, got: $this").left
  }

  def string: Yaml.Error \/ String = this match {
    case YamlString(s) => s.right
    case _ => Yaml.Error(s"Expected YamlString, got: $this").left
  }

  def int: Yaml.Error \/ Int = this match {
    case YamlInt(n) => n.right
    case _ => Yaml.Error(s"Expected YamlInt, got: $this}").left
  }

  def double: Yaml.Error \/ Double = this match {
    case YamlDouble(d) => d.right
    case _ => Yaml.Error(s"Expected YamlDouble, got: $this").left
  }

  def bool: Yaml.Error \/ Boolean = this match {
    case YamlBoolean(b) => b.right
    case _ => Yaml.Error(s"Expected YamlBoolean, got: $this").left
  }
}

sealed case class YamlAssoc(underlying: Map[String, Yaml]) extends Yaml
sealed case class YamlList(underlying: List[Yaml]) extends Yaml
sealed case class YamlString(underlying: String) extends Yaml
sealed case class YamlInt(underlying: Int) extends Yaml
sealed case class YamlDouble(underlying: Double) extends Yaml
sealed case class YamlBoolean(underlying: Boolean) extends Yaml
