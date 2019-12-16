package com.haskforce.highlighting.annotation.external.hsdev

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonReaderException, JsonValueCodec, JsonWriter}
import com.haskforce.highlighting.annotation.external.hsdev

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe.{TypeTag, typeTag, typeOf}

final class DisjointJsonCodecMap[A <: AnyRef : TypeTag] private (
  val toMap: ListMap[TypeTag[_], JsonValueCodec[_]]
) {
  override def toString: String = toMap.toString

  def toCodec: JsonValueCodec[A] = {
    new hsdev.DisjointJsonCodecMap.Codec[A](this)
  }
}

object DisjointJsonCodecMap {

  def apply[A <: AnyRef : TypeTag](
    kvs: KeyValue[_ <: A]*
  ): DisjointJsonCodecMap[A] = {
    new DisjointJsonCodecMap[A](ListMap(kvs.map(_.get): _*))
  }

  // Ensures that the class and the codec types match.
  final case class KeyValue[A <: AnyRef](
    get: (TypeTag[A], JsonValueCodec[A])
  ) extends AnyVal

  implicit def toKeyValue[A <: AnyRef : TypeTag](
    codec: JsonValueCodec[A]
  ): KeyValue[A] = new KeyValue[A]((
    typeTag[A],
    codec
  ))

  class Codec[A <: AnyRef : TypeTag](
    codecMap: DisjointJsonCodecMap[A]
  ) extends JsonValueCodec[A] {

    override def decodeValue(in: JsonReader, default: A): A = {
      codecMap.toMap.foreach { case (_, codec) =>
        in.setMark()
        try {
          codec.asInstanceOf[JsonValueCodec[A]].decodeValue(
            in, null.asInstanceOf[A]
          ) match {
            case x if x != null => return x.asInstanceOf[A]
            case _ => // noop
          }
        } catch {
          case e: JsonReaderException => // skip
        }
        in.rollbackToMark()
      }
      in.decodeError(
        s"All codecs failed to decode ${typeOf[A]}; " +
          s"codecs were: $codecMap"
      )
    }

    override def encodeValue(x: A, out: JsonWriter): Unit = {
      codecMap.toMap.get(typeTag[A]) match {
        case Some(codec) =>
          codec.asInstanceOf[JsonValueCodec[A]].encodeValue(x, out)
        case None =>
          throw new IllegalArgumentException(
            s"Class ${x.getClass} does not exist in codecs: $codecMap"
          )
      }
    }

    override def nullValue: A = null.asInstanceOf[A]
  }
}
