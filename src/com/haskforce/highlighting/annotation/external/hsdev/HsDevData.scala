package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.intellij.psi.PsiFile

final case class HsDevFileSource(
  file: String,
  contents: String
)

object HsDevFileSource {

  def fromPsiFile(x: PsiFile): HsDevFileSource = {
    // If the full source file path cannot be recovered (likely because
    // this is an in-memory PsiFile) then make up a source file name
    // based on the name of the PsiFile.
    val filePath =
      Option(x.getVirtualFile)
        .fold(s"${x.getName}.hs")(_.getCanonicalPath)
    HsDevFileSource(
      file = filePath,
      contents = x.getText
    )
  }

  implicit val jsonCodec: JsonValueCodec[HsDevFileSource] =
    JsonCodecMaker.make(CodecMakerConfig)
}


final case class HsDevModule(
  id: HsDevModuleId,
  exports: List[HsDevModuleExport]
)

object HsDevModule {
  implicit val jsonCodec: JsonValueCodec[HsDevModule] =
    JsonCodecMaker.make(CodecMakerConfig)
}

final case class HsDevModuleId(
  name: String,
  location: HsDevModuleLocation
)

sealed trait HsDevModuleLocation
object HsDevModuleLocation {
  final case class FileModule(
    file: String,
    project: Option[HsDevProject]
  ) extends HsDevModuleLocation

  final case class InstalledModule(
    dirs: List[String],
    `package`: String,
    name: String,
    exposed: Boolean
  ) extends HsDevModuleLocation

  final case class OtherLocation(
    source: String
  ) extends HsDevModuleLocation

  // This could be a case object, but this makes it simpler to derive
  // as an empty object.
  final case class NoLocation() extends HsDevModuleLocation

//  private case class Raw(
//    // FileModule
//    file: Option[String],
//    project: Option[HsDevProject],
//    // InstalledModule
//    dirs: Option[List[String]],
//    `package`: Option[String],
//    name: Option[String],
//    exposed: Option[Boolean],
//    // OtherLocation
//    source: Option[String]
//  )

  private val fileModuleCodec: JsonValueCodec[FileModule] =
    JsonCodecMaker.make(CodecMakerConfig)

  private val installedModuleCodec: JsonValueCodec[InstalledModule] =
    JsonCodecMaker.make(CodecMakerConfig)

  private val otherLocationCodec: JsonValueCodec[OtherLocation] =
    JsonCodecMaker.make(CodecMakerConfig)

  private val noLocationCodec: JsonValueCodec[NoLocation] =
    JsonCodecMaker.make(CodecMakerConfig)

//  private val rawCodec: JsonValueCodec[Raw] =
//    JsonCodecMaker.make(CodecMakerConfig)

  private def decodeValueAlt[A](
    name: String,
    in: JsonReader,
    codecs: JsonValueCodec[_ <: A]*
  ): A = {
    codecs.foreach { codec =>
      in.setMark()
      decodeValueOrNull(in, codec) match {
        case x if x != null => return x
      }
      in.rollbackToMark()
    }
    in.decodeError(s"All codecs failed to decode $name")
  }

  private def decodeValueOrNull[A](
    in: JsonReader,
    codec: JsonValueCodec[A]
  ): A = {
    codec.decodeValue(in, null.asInstanceOf[A])
  }

  implicit val jsonCodec: JsonValueCodec[HsDevModuleLocation] =
    new JsonValueCodec[HsDevModuleLocation] {
      override def decodeValue(in: JsonReader, default: HsDevModuleLocation): HsDevModuleLocation = {
        decodeValueAlt[HsDevModuleLocation](
          "HsDevModuleLocation",
          in,
          fileModuleCodec,
          installedModuleCodec,
          otherLocationCodec,
          noLocationCodec
        )

//        val raw = rawCodec.decodeValue(in, null)
//        if (raw == null) in.decodeError("Invalid variant for HsDevModuleLocation")
//
//        ( // FileModule
//          for {
//            file <- raw.file
//            project = raw.project
//          } yield FileModule(file = file, project = project)
//        ).foreach(return _)
//
//        ( // InstalledModule
//          for {
//            dirs <- raw.dirs
//            pkg <- raw.`package`
//            name <- raw.name
//            exposed <- raw.exposed
//          } yield InstalledModule(
//            dirs = dirs,
//            `package` = pkg,
//            name = name,
//            exposed = exposed
//          )
//        ).foreach(return _)
//
//        ( // OtherLocation
//          for {
//            source <- raw.source
//          } yield OtherLocation(source = source)
//        ).foreach(return _)
//
//        // default if nothing else was parsed, because any object
//        // (already parsed as `raw`) matches the empty object.
//        NoLocation
      }

      override def encodeValue(x: HsDevModuleLocation, out: JsonWriter): Unit = {
        x match {
          case m: FileModule => fileModuleCodec.encodeValue(m, out)
          case m: InstalledModule => installedModuleCodec.encodeValue(m, out)
          case m: OtherLocation => otherLocationCodec.encodeValue(m, out)
          case m: NoLocation => noLocationCodec.encodeValue(m, out)
        }
      }

      override def nullValue: HsDevModuleLocation = {
        // Hack, mostly to avoid having to throw RuntimeException
        fileModuleCodec.nullValue
      }
    }
}

final case class HsDevProject(
  name: String,
  path: String,
  cabal: String,
  // description: HsDevProjectDescription
)

// final case class HsDevProjectDescription(
//   version: String,
//   library: Option[HsDevLibrary],
//   executables: List[HsDevExecutable],
//   tests: List[HsDevTest]
// )

final case class HsDevLibrary(
  modules: List[List[String]],
  // buildInfo: HsDevInfo
)

// final case class HsDevInfo(
//   `build-depends`: List[String],
//   language: Option[HsDevLanguage],
//   extensions: List[HsDevExtension],
//   `ghc-options`: List[String],
//   `source-dirs`: List[String],
//   `other-modules`: List[List[String]]
// )

final case class HsDevModuleExport(
  id: HsDevModuleExportId,
  docs: String,
  info: HsDevModuleExportInfo
)

final case class HsDevModuleExportInfo(
  what: String,
  `type`: String
)

final case class HsDevModuleExportId(
  name: String,
  module: HsDevModuleId
)

final case class HsDevNote[A](
  source: HsDevModuleLocation,
  region: HsDevRegion,
  level: Option[HsDevSeverity],
  note: A
)

sealed trait HsDevSeverity
object HsDevSeverity {
  case object error extends HsDevSeverity
  case object warning extends HsDevSeverity
  case object hint extends HsDevSeverity

  implicit val jsonCodec: JsonValueCodec[HsDevSeverity] =
    JsonCodecMaker.make(CodecMakerConfig.withDiscriminatorFieldName(None))
}

object HsDevNote {
  implicit val jsonCodecOutputMessage: JsonValueCodec[HsDevNote[HsDevOutputMessage]] =
    JsonCodecMaker.make(CodecMakerConfig)
}

final case class HsDevRegion(
  from: HsDevPosition,
  to: HsDevPosition
)

final case class HsDevPosition(
  line: Int,
  column: Int
)

final case class HsDevOutputMessage(
  message: String,
  suggestion: Option[String]
)

final case class HsDevNoteNote(
  suggestion: Option[String],
  message: String
)
