package com.haskforce.highlighting.annotation.external.hsdev

import java.io.{BufferedInputStream, InputStream}
import java.nio.charset.StandardCharsets

import com.github.plokhotnyuk.jsoniter_scala.{core => Jsoniter}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.intellij.psi.PsiFile

import scala.collection.mutable
import scala.util.control.NonFatal

object HsDevData {

  def fromJSONArrayStream[A](
    is: InputStream
  )(implicit codec: JsonValueCodec[A]
  ): Either[HsDevDataException, Vector[A]] = {
    val bis = new BufferedInputStream(is)
    peekDecodeHsDevException(bis) match {
      case Some(e) => Left(e)
      case None =>
        try {
          val buf = new mutable.ArrayBuffer[A]
          Jsoniter.scanJsonArrayFromStream[A](bis) { a: A => buf += a ; true }
          Right(buf.toVector)
        } catch {
          case NonFatal(e) =>
            Left(
              new HsDevDataException(
                s"Failed to decode from stream: $e",
                e
              )
            )
        }
    }
  }

  /**
   * Attempts to decode an HsDevError from the given stream.
   * Assumes that the input stream will be an HsDevError if it
   * encounters a JSON object (e.g. starts with a '{' char).
   * Otherwise, returns None.
   */
  private def peekDecodeHsDevException(
    bis: BufferedInputStream
  ): Option[HsDevDataException] = {
    bis.mark(1)
    val char0 = bis.read()
    bis.reset()
    if (char0 != '{') None else {
      try {
        Some(new HsDevDataException(Jsoniter.readFromStream[HsDevError](bis)))
      } catch {
        case NonFatal(e) =>
          val buf = new Array[Byte](1000)
          bis.read(buf)
          Some(new HsDevDataException(s"Failed to decode HsDevError: $e", e))
      }
    }
  }
}

final class HsDevDataException private (
  error: Either[String, HsDevError],
  cause: Throwable = null
) extends RuntimeException(cause) {
  def this(s: String, e: Throwable) = this(Left(s), e)
  def this(e: HsDevError) = this(Right(e))
  override def getMessage: String = error.map(_.toString).merge
  override def toString: String = s"HsDevException($getMessage)"
}

final case class HsDevError(
  error: String,
  msg: String
)

object HsDevError {
  implicit val jsonCodec: JsonValueCodec[HsDevError] =
    JsonCodecMaker.make[HsDevError](CodecMakerConfig)
}

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

  implicit val jsonCodec: JsonValueCodec[HsDevModuleLocation] =
    DisjointJsonCodecMap[HsDevModuleLocation](
      JsonCodecMaker.make[FileModule](CodecMakerConfig),
      JsonCodecMaker.make[InstalledModule](CodecMakerConfig),
      JsonCodecMaker.make[OtherLocation](CodecMakerConfig),
      JsonCodecMaker.make[NoLocation](CodecMakerConfig)
    ).toCodec
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
