package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

final case class HsDevModule(
  id: HsDevModuleId,
  exports: List[HsDevModuleExport]
)

object HsDevModule {
  implicit val jsonCodec: JsonValueCodec[HsDevModule] =
    JsonCodecMaker.make[HsDevModule](CodecMakerConfig)
}

final case class HsDevModuleId(
  name: String,
  location: HsDevModuleLocation
)

final case class HsDevModuleLocation(
  dirs: List[String],
  exposed: Boolean,
  name: String,
  `package`: String
)

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
