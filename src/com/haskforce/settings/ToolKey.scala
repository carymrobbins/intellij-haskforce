package com.haskforce.settings

import com.haskforce.utils.NotificationUtil
import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger

import scala.reflect.{ClassTag, classTag}
import scala.util.control.NonFatal

trait ToolKey[A] {
  def name: String
  def getValue(props: PropertiesComponent): A
}

final case class PathToolKey(prefix: String)
  extends ToolKey[Option[String]]
  with ToolKey.Setter[String] {

  val name = s"${prefix}Path"

  override def getValue(props: PropertiesComponent): Option[String] = {
    Option(props.getValue(name)).filter(_.nonEmpty)
  }

  override def setValue(props: PropertiesComponent, a: String): Unit = {
    props.setValue(name, a)
  }
}

final case class FlagsToolKey(prefix: String)
  extends ToolKey[String]
  with ToolKey.Setter[String] {

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
) extends ParseToolKeyWithDefault[Boolean](_.toBoolean, name, getDefault)
  with ToolKey.Setter[Boolean] {

  override def setValue(props: PropertiesComponent, a: Boolean): Unit = {
    props.setValue(name, a.toString)
  }

  // Java interop thinks the generic methods return Object, possibly due to the
  // weirdness of scala primitives, so giving explicit types here to help.
  def getBool(props: PropertiesComponent): Boolean = getValue(props)
  def setBool(props: PropertiesComponent, a: Boolean): Unit = setValue(props, a)
}

class ParseToolKey[A : ClassTag](
  private val parse: String => A,
  name: String
) extends ParseToolKeyWithDefault[Option[A]](s => Option(parse(s)), name, _ => None)

class ParseToolKeyWithDefault[A : ClassTag](
  private val parse: String => A,
  val name: String,
  val getDefault: PropertiesComponent => A
) extends ToolKey[A]
  with ToolKey.HasDefault[A] {

  override def getValue(props: PropertiesComponent): A = {
    lazy val default = getDefault(props)
    (
      try {
        Option(props.getValue(name)).map(parse)
      } catch {
        case NonFatal(e) =>
          val message = s"Failed to parse property $name as ${classTag[A].runtimeClass}, using default: $default"
          ToolKey.LOG.warn(message, e)
          NotificationUtil.displaySimpleNotification(NotificationType.WARNING, null, "Configuration", message)
          None
      }
    ).getOrElse(default)
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

  trait Setter[A] {
    def setValue(props: PropertiesComponent, a: A): Unit
  }

  trait Unsetter[A] extends Setter[A] {
    def unsetValue(props: PropertiesComponent): Unit

    final def setValue(props: PropertiesComponent, oa: Option[A]): Unit = {
      oa match {
        case None => unsetValue(props)
        case Some(a) => setValue(props, a)
      }
    }
  }

  val LOG: Logger = Logger.getInstance(ToolKey.getClass)

  val STYLISH_HASKELL = SimpleToolSettingsKey("stylishHaskell")
  val HLINT = SimpleToolSettingsKey("hlint")
  val HINDENT = SimpleToolSettingsKey("hindent")
  val GHC_MOD = SimpleToolSettingsKey("ghcMod")

  object GHC_MODI extends AbstractSimpleToolSettingsKey[GhcModiToolSettings]("ghcModi") {
    val RESPONSE_TIMEOUT_MS = new ParseToolKeyWithDefault[Long](_.toLong, s"${name}Timeout", _ => 5000) // 5 seconds
    val KILL_IDLE_TIMEOUT_MS = new ParseToolKeyWithDefault[Long](_.toLong, s"${name}KillIdleTimeout", _ => 600000) // 10 minutes

    override def getValue(props: PropertiesComponent): GhcModiToolSettings = {
      GhcModiToolSettings(
        path = PATH.getValue(props),
        flags = FLAGS.getValue(props),
        responseTimeoutMS = RESPONSE_TIMEOUT_MS.getValue(props),
        killIdleTimeoutMS = KILL_IDLE_TIMEOUT_MS.getValue(props)
      )
    }
  }

  final case class GhcModiToolSettings(
    path: Option[String],
    flags: String,
    responseTimeoutMS: Long,
    killIdleTimeoutMS: Long
  ) extends SimpleToolSettings

  object HSDEV extends AbstractSimpleToolSettingsKey[HsDevToolSettings]("hsdev") {
    val ENABLED = BooleanToolKeyWithDefault(s"${name}Enabled", PATH.getValue(_).isDefined)
    val SCAN_TIMEOUT_SECONDS = new ParseToolKey[Long](_.toLong, s"${name}ScanTimeout")
    val COMMAND_TIMEOUT_SECONDS = new ParseToolKey[Long](_.toLong, s"${name}CommandTimeout")
    val PORT = new ParseToolKey[Int](_.toInt, s"${name}Port")
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
