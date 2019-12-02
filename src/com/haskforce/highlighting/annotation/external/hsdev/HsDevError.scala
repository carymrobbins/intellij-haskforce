package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

case class HsDevError(
  error: String,
  msg: String
)

object HsDevError {
  implicit val jsonCodec: JsonValueCodec[HsDevError] =
    JsonCodecMaker.make[HsDevError](CodecMakerConfig)
}
