package com.haskforce.ui.status

import java.awt.Point
import java.awt.event.MouseEvent

import com.haskforce.HaskellIcons
import com.haskforce.highlighting.annotation.external.hsdev.HsDevProjectComponent
import com.haskforce.settings.{HaskellToolsConfigurable, ToolKey}
import com.intellij.ide.DataManager
import com.intellij.ide.plugins.PluginManager
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.extensions.PluginId
import com.intellij.openapi.options.ShowSettingsUtil
import com.intellij.openapi.project.{DumbAwareAction, DumbAwareToggleAction, Project}
import com.intellij.openapi.ui.popup.{JBPopupFactory, ListPopup}
import com.intellij.openapi.wm.{StatusBar, StatusBarWidget, StatusBarWidgetProvider}
import com.intellij.ui.awt.RelativePoint
import com.intellij.util.Consumer
import javax.swing.{Icon, SwingConstants}

import scala.reflect.{ClassTag, classTag}

class HaskForceStatusBarIconProvider extends StatusBarWidgetProvider {
  override def getWidget(project: Project): StatusBarWidget = {
    HaskForceStatusBarWidget
  }
}

case object HaskForceStatusBarWidget
  extends StatusBarWidget
  with StatusBarWidget.IconPresentation {

  private val LOG = Logger.getInstance(getClass)

  private var statusBar: Option[StatusBar] = None

  // Uses the `toString` derived from `case object`
  override def ID(): String = toString

  override def getPresentation(
    typ: StatusBarWidget.PlatformType
  ): StatusBarWidget.WidgetPresentation = this

  override def install(statusBar: StatusBar): Unit = {
    this.statusBar = Some(statusBar)
  }

  override def getIcon: Icon = HaskellIcons.FILE

  override def dispose(): Unit = {}

  override def getTooltipText: String = "HaskForce actions"

  override def getClickConsumer: Consumer[MouseEvent] = { event =>
    val component = event.getComponent
    val popup =
      ActionsPopup
        .getPopup(DataManager.getInstance().getDataContext(component))
    val dimension = popup.getContent.getPreferredSize
    val at = new Point(0, -dimension.height)
    popup.show(new RelativePoint(component, at))
  }

  private object ActionsPopup {

    private val TITLE = "HaskForce"

    def getPopup(dataContext: DataContext): ListPopup = {
      val actions = buildActions(dataContext)
      val popup =
        JBPopupFactory
          .getInstance()
          .createActionGroupPopup(
            TITLE, actions, dataContext,
            JBPopupFactory.ActionSelectionAid.SPEEDSEARCH,
            false,
            HaskForceStatusBarWidget.ID()
          )
      popup.setAdText(s"Version $pluginVersion", SwingConstants.CENTER)
      popup
    }

    private lazy val pluginVersion: String = {
      PluginManager
        .getPlugin(PluginId.getId("com.haskforce"))
        .getVersion
    }

    private def buildActions(dataContext: DataContext): DefaultActionGroup = {
      val optProject = Option(dataContext.getData(CommonDataKeys.PROJECT))
      val optHsDevProjectComponent = optProject.flatMap(HsDevProjectComponent.get)
      val actions = new DefaultActionGroup
      actions.setPopup(true)
      val manager = ActionManager.getInstance()

      // Assumes the action id is the canonical class name by convention.
      def getAction[A : ClassTag]: AnAction = {
        manager.getAction(classTag[A].runtimeClass.getCanonicalName)
      }

      actions.add(getAction[ConfigureHaskellToolsAction])
      optHsDevProjectComponent.foreach { _ =>
        actions.addSeparator("HsDev")
        actions.add(getAction[HsDevToggleEnabledAction])
        actions.add(getAction[HsDevRestartServerAction])
      }
      actions
    }
  }
}

class ConfigureHaskellToolsAction extends DumbAwareAction {
  override def actionPerformed(e: AnActionEvent): Unit = {
    ShowSettingsUtil.getInstance.showSettingsDialog(
      e.getProject,
      HaskellToolsConfigurable.HASKELL_TOOLS_ID
    )
  }
}

class HsDevToggleEnabledAction extends DumbAwareToggleAction {

  override def update(e: AnActionEvent): Unit = {
    e.getPresentation.setText(
      e.getPlace match {
        // Displayed in the status icon menu.
        case s if s == HaskForceStatusBarWidget.ID() =>
          if (isSelected(e)) "Enabled" else "Enable"
        // Displayed in the top menu.
        case _ => "Enable HsDev"
      }
    )
  }

  override def isSelected(e: AnActionEvent): Boolean = {
    ToolKey.HSDEV.ENABLED.getValue(PropertiesComponent.getInstance(e.getProject))
  }

  override def setSelected(e: AnActionEvent, state: Boolean): Unit = {
    ToolKey.HSDEV.ENABLED.setValue(
      PropertiesComponent.getInstance(e.getProject),
      state
    )
    if (state) {
      HsDevProjectComponent.get(e.getProject).foreach(_.restart())
    } else {
      HsDevProjectComponent.get(e.getProject).foreach(_.kill())
    }
  }
}

class HsDevRestartServerAction extends DumbAwareAction {

  override def actionPerformed(e: AnActionEvent): Unit = {
    HsDevProjectComponent.get(e.getProject).foreach(_.restart())
  }

  override def update(e: AnActionEvent): Unit = {
    e.getPresentation.setText(
      e.getPlace match {
        // Displayed in the status icon menu.
        case s if s == HaskForceStatusBarWidget.ID() => "Restart Server"
        // Displayed in the top menu.
        case _ => "Restart HsDev server"
      }
    )
  }
}
