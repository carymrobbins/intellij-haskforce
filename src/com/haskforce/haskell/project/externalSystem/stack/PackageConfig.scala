package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.cabal.query.{BuildInfo, CabalQuery}
import com.haskforce.utils._
import com.intellij.openapi.application.ApplicationManager
import prelude._
import scalaz.std.list._

final case class PackageConfig(
  name: String,
  components: List[PackageConfig.Component]
)

object PackageConfig {

  def fromFile(file: File): Either[Throwable, Option[PackageConfig]] = {
    val name = file.getName
    if (!name.endsWith(".cabal")) return Right(None)
    val packageName = name.stripSuffix(".cabal")
    for {
      cabalFile <- PsiFileParser.parseForDefaultProject[CabalFile, File](file)
      packageConfig <- fromCabalFile(
        packageName = packageName,
        cabalFile = cabalFile
      )
    } yield Option(packageConfig)
  }

  // NOTE: We can't depend on 'getName' or 'getVirtualFile' of 'cabalFile' here
  // because the 'cabalFile' was parsed via text, so it has no name or file
  // path; thus, we explicitly supply the 'packageName' here.
  private def fromCabalFile(
    packageName: String,
    cabalFile: CabalFile
  ): Either[Throwable, PackageConfig] = {
    Either.catchNonFatal {
      fromCabalQuery(
        packageName = packageName,
        q = new CabalQuery(SPsiFile(cabalFile))
      ).run(ApplicationManager.getApplication)
    }
  }

  private def fromCabalQuery(
    packageName: String,
    q: CabalQuery
  ): IJReadAction[PackageConfig] = {
    for {
      buildInfos <- q.getBuildInfo
      components <- buildInfos.toList.traverse(mkComponent(packageName, _))
    } yield PackageConfig(
      packageName,
      components
    )
  }

  private def mkComponent(
    packageName: String,
    buildInfo: BuildInfo
  ): IJReadAction[Component] = {
    val typ = Component.Type.fromBuildInfoType(buildInfo.typ)
    for {
      name <- (buildInfo match {
        case _: BuildInfo.Library => IJReadAction(Option(packageName))
        case x: BuildInfo.Executable => x.getName
        case x: BuildInfo.TestSuite => x.getName
        case x: BuildInfo.Benchmark => x.getName
      }).map(_.getOrElse {
        throw new InvalidPackageConfigException(
          s"Failed to get 'name' for $typ component of package $packageName"
        )
      })
      hsSourceDirs <- buildInfo.getSourceDirs
      mainIs <- buildInfo match {
        case _: BuildInfo.Library => IJReadAction(None)
        case x: BuildInfo.Executable => x.getMainIs
        case x: BuildInfo.TestSuite => x.getMainIs
        case x: BuildInfo.Benchmark => x.getMainIs
      }
      dependencies <- buildInfo.getDependencies
      extensions <- buildInfo.getExtensions
    } yield Component(
      typ = typ,
      name = name,
      hsSourceDirs = hsSourceDirs,
      mainIs = mainIs,
      dependencies = dependencies,
      extensions = extensions
    )
  }

  final case class Component(
    typ: Component.Type,
    name: String,
    hsSourceDirs: NonEmptySet[String],
    mainIs: Option[String],
    dependencies: Set[String],
    extensions: Set[String]
  )

  object Component {
    sealed trait Type
    object Type {

      case object Library extends Type
      case object Executable extends Type
      case object TestSuite extends Type
      case object Benchmark extends Type

      private val ALL = List[Type](Library, Executable, TestSuite, Benchmark)

      def fromBuildInfoType(t: BuildInfo.Type): Type = t match {
        case BuildInfo.Type.Library => Library
        case BuildInfo.Type.Executable => Executable
        case BuildInfo.Type.TestSuite => TestSuite
        case BuildInfo.Type.Benchmark => Benchmark
      }

      implicit val jdom: JDOMFieldExternalizable[Type] = {
        JDOMFieldExternalizable.string.imap(
          s => {
            ALL.find(_.toString == s).getOrElse {
              throw new IllegalArgumentException(s"Invalid Component.Type: $s")
            }
          },
          _.toString
        )
      }
    }

    implicit val jdom: JDOMExternalizable[Component] =
      JDOMExternalizable.derive6(apply, unapply)
  }

  final case class Dependency(
    name: String,
    version: String
  )

  implicit val jdom: JDOMFieldExternalizable[PackageConfig] =
    JDOMFieldExternalizable.fromJDOMExternalizable(
      JDOMExternalizable.derive2(apply, unapply)
    )
}

final class InvalidPackageConfigException(override val getMessage: String)
  extends RuntimeException
