package com.haskforce.settings

import com.haskforce.utils.NotificationUtil
import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.util.messages.Topic

import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.util.control.NonFatal

trait ToolKey[A] {
  def name: String
  def getValue(props: PropertiesComponent): A
  def setValue(props: PropertiesComponent, a: A): Unit
}

final case class PathToolKey(prefix: String)
  extends ToolKey[Option[String]] {

  val name = s"${prefix}Path"

  override def getValue(props: PropertiesComponent): Option[String] = {
    Option(props.getValue(name)).filter(_.nonEmpty)
  }

  override def setValue(props: PropertiesComponent, a: Option[String]): Unit = {
    a.fold(props.setValue(name, ""))(props.setValue(name, _))
  }
}

final case class FlagsToolKey(prefix: String)
  extends ToolKey[String] {

  val name = s"${prefix}Flags"

  override def getValue(props: PropertiesComponent): String = {
    Option(props.getValue(name)).getOrElse("")
  }

  override def setValue(props: PropertiesComponent, a: String): Unit = {
    props.setValue(name, a)
  }
}

final case class BooleanToolKeyWithDefault(
  override val name: String,
  override val getDefault: PropertiesComponent => Boolean
) extends CodecToolKeyWithDefault[Boolean](_.toBoolean, _.toString, name, getDefault) {
  // Java interop thinks the generic methods return Object, possibly due to the
  // weirdness of scala primitives, so giving explicit types here to help.
  def getBool(props: PropertiesComponent): Boolean = getValue(props)
  def setBool(props: PropertiesComponent, a: Boolean): Unit = setValue(props, a)
}

final case class LongToolKeyWithDefault(
  override val name: String,
  override val getDefault: PropertiesComponent => Long
) extends CodecToolKeyWithDefault[Long](_.toLong, _.toString, name, getDefault) {
  // Java interop thinks the generic methods return Object, possibly due to the
  // weirdness of scala primitives, so giving explicit types here to help.
  def getLong(props: PropertiesComponent): Long = getValue(props)
  def setLong(props: PropertiesComponent, a: Long): Unit = setValue(props, a)
}

class CodecToolKey[A : TypeTag](
  decodeOrThrow: String => A,
  encode: A => String,
  name: String
) extends CodecToolKeyWithDefault[Option[A]](
  // Interpret empty strings as None.
  decodeOrThrow = s => Option(s).filter(_.nonEmpty).map(decodeOrThrow),
  encode = _.map(encode).orNull,
  name = name,
  getDefault = _ => None
)

class CodecToolKeyWithDefault[A : TypeTag](
  val decodeOrThrow: String => A,
  val encode: A => String,
  val name: String,
  val getDefault: PropertiesComponent => A
) extends ToolKey[A]
  with ToolKey.HasDefault[A]
  with ToolKey.Decoder[A] {

  override def getValue(props: PropertiesComponent): A = {
    // Consider empty strings the same as null.
    val rawValue = Option(props.getValue(name)).filter(_.nonEmpty)
    rawValue.map(parseString) match {
      case None => getDefault(props)
      case Some(Right(a)) => a
      case Some(Left(e)) =>
        val d = getDefault(props)
        val input = rawValue.fold("null")(s => s""""$s"""")
        val message = s"Failed to parse property $name as ${typeOf[A]}, using default: $d; input was $input"
        ToolKey.LOG.warn(message, e)
        NotificationUtil.displaySimpleNotification(NotificationType.WARNING, null, "Configuration", message)
        d
    }
  }

  override def parseString(s: String): Either[Throwable, A] = {
    try {
      Right(decodeOrThrow(s))
    } catch {
      case NonFatal(e) => Left(e)
    }
  }

  override def setValue(props: PropertiesComponent, a: A): Unit = {
    Option(encode(a)) match {
      case None => props.unsetValue(name)
      case Some(a) =>  props.setValue(name, a)
    }
  }
}

abstract class AbstractSimpleToolSettingsKey[A <: SimpleToolSettings](
  override val name: String
) extends ToolKey[A] {
  val PATH = PathToolKey(name)
  val FLAGS = FlagsToolKey(name)
}

final case class SimpleToolSettingsKey(
  override val name: String
) extends AbstractSimpleToolSettingsKey[SimpleToolSettings](name) {
  override def getValue(props: PropertiesComponent): SimpleToolSettings = {
    SimpleToolSettings.Default(
      path = PATH.getValue(props),
      flags = FLAGS.getValue(props)
    )
  }

  override def setValue(props: PropertiesComponent, a: SimpleToolSettings): Unit = {
    PATH.setValue(props, a.path)
    FLAGS.setValue(props, a.flags)
  }
}

trait SimpleToolSettings {
  def path: Option[String]
  def flags: String
}

object SimpleToolSettings {
  final case class Default(path: Option[String], flags: String) extends SimpleToolSettings
}

object ToolKey {

  trait HasDefault[A] {
    def getDefault: PropertiesComponent => A
  }

  trait Decoder[A] {
    def parseString(s: String): Either[Throwable, A]
  }

  trait NotifyChanged {
    def notifyChanged(project: Project): Unit
  }

  object NotifyChanged {
    def of[A, S <: SettingsChangeNotifier[A]](
      project: Project,
      topic: Topic[S],
      toolKey: ToolKey[A]
    ): Unit = {
      project.getMessageBus
        .syncPublisher(topic)
        .onSettingsChanged(
          toolKey.getValue(PropertiesComponent.getInstance(project))
        )
    }
  }

  val LOG: Logger = Logger.getInstance(ToolKey.getClass)

  val STYLISH_HASKELL = SimpleToolSettingsKey("stylishHaskell")
  val HLINT = SimpleToolSettingsKey("hlint")
  val HINDENT = SimpleToolSettingsKey("hindent")
  val GHC_MOD = SimpleToolSettingsKey("ghcMod")

  object GHC_MODI
    extends AbstractSimpleToolSettingsKey[GhcModiToolSettings]("ghcModi")
    with NotifyChanged {

    val RESPONSE_TIMEOUT_MS = LongToolKeyWithDefault(s"${name}Timeout", _ => 5000) // 5 seconds
    val KILL_IDLE_TIMEOUT_MS = LongToolKeyWithDefault(s"${name}KillIdleTimeout", _ => 600000) // 10 minutes

    override def getValue(props: PropertiesComponent): GhcModiToolSettings = {
      GhcModiToolSettings(
        path = PATH.getValue(props),
        flags = FLAGS.getValue(props),
        responseTimeoutMS = RESPONSE_TIMEOUT_MS.getValue(props),
        killIdleTimeoutMS = KILL_IDLE_TIMEOUT_MS.getValue(props)
      )
    }

    override def setValue(props: PropertiesComponent, a: GhcModiToolSettings): Unit = {
      PATH.setValue(props, a.path)
      FLAGS.setValue(props, a.flags)
      RESPONSE_TIMEOUT_MS.setValue(props, a.responseTimeoutMS)
      KILL_IDLE_TIMEOUT_MS.setValue(props, a.killIdleTimeoutMS)
    }

    override def notifyChanged(project: Project): Unit = NotifyChanged.of(
      project, SettingsChangeNotifier.GHC_MODI_TOPIC, this
    )
  }

  final case class GhcModiToolSettings(
    path: Option[String],
    flags: String,
    responseTimeoutMS: Long,
    killIdleTimeoutMS: Long
  ) extends SimpleToolSettings

  object HSDEV
    extends AbstractSimpleToolSettingsKey[HsDevToolSettings]("hsdev")
    with NotifyChanged {

    val ENABLED = BooleanToolKeyWithDefault(s"${name}Enabled", PATH.getValue(_).isDefined)
    val SCAN_TIMEOUT_SECONDS = new CodecToolKey[Long](_.toLong, _.toString, s"${name}ScanTimeout")
    val COMMAND_TIMEOUT_SECONDS = new CodecToolKey[Long](_.toLong, _.toString, s"${name}CommandTimeout")
    val PORT = new CodecToolKey[Int](_.toInt, _.toString, s"${name}Port")
    val SPAWN_SERVER = BooleanToolKeyWithDefault(s"${name}SpawnServer", _ => true)

    override def getValue(props: PropertiesComponent): HsDevToolSettings = {
      HsDevToolSettings(
        path = PATH.getValue(props),
        flags = FLAGS.getValue(props),
        enabled = ENABLED.getValue(props),
        scanTimeoutSeconds = SCAN_TIMEOUT_SECONDS.getValue(props),
        commandTimeoutSeconds = SCAN_TIMEOUT_SECONDS.getValue(props),
        port = PORT.getValue(props),
        spawnServer = SPAWN_SERVER.getValue(props)
      )
    }

    override def setValue(props: PropertiesComponent, a: HsDevToolSettings): Unit = {
      PATH.setValue(props, a.path)
      FLAGS.setValue(props, a.flags)
      ENABLED.setValue(props, a.enabled)
      SCAN_TIMEOUT_SECONDS.setValue(props, a.scanTimeoutSeconds)
      COMMAND_TIMEOUT_SECONDS.setValue(props, a.commandTimeoutSeconds)
      PORT.setValue(props, a.port)
      SPAWN_SERVER.setValue(props, a.spawnServer)
    }

    def notifyChanged(project: Project): Unit = NotifyChanged.of(
      project, SettingsChangeNotifier.HSDEV_TOPIC, this
    )
  }

  final case class HsDevToolSettings(
    path: Option[String],
    flags: String,
    enabled: Boolean,
    scanTimeoutSeconds: Option[Long],
    commandTimeoutSeconds: Option[Long],
    port: Option[Int],
    spawnServer: Boolean
  ) extends SimpleToolSettings
}
