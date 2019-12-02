package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
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

  final case class NoLocation() extends HsDevModuleLocation

  implicit val jsonCodec: JsonValueCodec[HsDevModuleLocation] =
    JsonCodecMaker.make(CodecMakerConfig)
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
