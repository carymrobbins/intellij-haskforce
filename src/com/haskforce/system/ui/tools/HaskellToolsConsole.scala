package com.haskforce.system.ui.tools

import java.awt.event.ItemEvent
import java.awt.{GridBagLayout, GridLayout}
import java.util.regex.Pattern
import javax.swing.{JComponent, JPanel}

import scala.collection.mutable

import com.intellij.execution.filters.TextConsoleBuilderFactory
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.Disposable
import com.intellij.openapi.actionSystem.{ActionManager, DefaultActionGroup}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.ModuleUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.SimpleToolWindowPanel
import com.intellij.openapi.util.Condition
import com.intellij.openapi.wm.{ToolWindow, ToolWindowFactory}
import com.intellij.ui.content.ContentFactory

import com.haskforce.haskell.HaskellModuleType
import com.haskforce.system.settings.ToolKey
import com.haskforce.system.ui.{GC, SComboBox}
import com.haskforce.system.utils.SAMUtils

/**
  * Manages the Haskell Tools Console "tool window" for a project.
  *
  * Tools can use the write* methods to communicate command input, output, and errors.
  * This enables users to see and understand exactly what is occurring and debug any issues.
  */
final class HaskellToolsConsole private(project: Project) {

  val LINE_LIMIT = 3
  val TRUNCATED_SUFFIX = "\n<<truncated output...>>\n"

  def curry(toolKey: ToolKey): HaskellToolsConsole.Curried = {
    new HaskellToolsConsole.Curried(this, toolKey)
  }

  /** Log tool input to the console. */
  def writeInput(toolKey: ToolKey, msg: String): Unit = {
    write(ConsoleViewContentType.USER_INPUT, toolKey, msg)
  }

  /** Log tool output to the console. */
  def writeOutput(toolKey: ToolKey, msg: String): Unit = {
    write(ConsoleViewContentType.NORMAL_OUTPUT, toolKey, msg)
  }

  /** Log a tool error to the console. */
  def writeError(toolKey: ToolKey, msg: String): Unit = {
    write(ConsoleViewContentType.ERROR_OUTPUT, toolKey, msg)
  }

  private def write(contentType: ConsoleViewContentType, toolKey: ToolKey, msg: String): Unit = {
    ApplicationManager.getApplication.invokeLater(SAMUtils.runnable {
      val m = if (msg.isEmpty) "<empty message>" else msg
      getConsole(toolKey).view.print(m + "\n", contentType)
    })
  }

  /** Component used for the tool window. */
  lazy val component = new JPanel(new GridBagLayout)

  /** Initialize the UI; should only by called by the ToolWindowFactory. */
  def initUI(): Unit = {
    toolsCombo.setEditable(false)
    toolsCombo.setRenderer(SAMUtils.listCellRenderer((_: ToolKey).prettyName))

    component.add(toolsCombo, toolsComboGC)
    showConsole()

    toolsCombo.addItemListener(SAMUtils.itemListener { e =>
      if (e.getStateChange == ItemEvent.SELECTED) showConsole()
    })
  }

  // Constraints for setting up the UI.
  private lazy val baseGC = GC.pad(10, 5).northWest
  private lazy val toolsComboGC = baseGC.grid(0, 0)
  private lazy val consoleGC = baseGC.grid(0, 1).weight(1, 1).fillBoth

  /** Combo box to choose a tool. */
  private lazy val toolsCombo = new SComboBox[ToolKey]

  /** Consoles for each tool. */
  private val consoles = mutable.Map[ToolKey, ToolConsoleView]()

  /** The currently visible console. */
  private[this] var currentConsole: Option[ToolConsoleView] = None

  /** Display the currently selected console. */
  private def showConsole(): Unit = {
    selectedToolKey().foreach { toolKey =>
      val console = getConsole(toolKey)
      if (currentConsole.contains(console)) return
      currentConsole.foreach { c => component.remove(c.getComponent) }
      currentConsole = Some(console)
      component.add(console.getComponent, consoleGC)
      component.revalidate()
      component.repaint()
    }
  }

  /** Get the console for a tool. */
  private def getConsole(toolKey: ToolKey): ToolConsoleView = {
    consoles.getOrElse(toolKey, newConsole(toolKey))
  }

  private val consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(project)

  /** Create a console for a tool. */
  private def newConsole(toolKey: ToolKey): ToolConsoleView = {
    if (consoles.contains(toolKey)) throw new RuntimeException(s"Tool console already exists: $toolKey")
    val c = ToolConsoleView.create(consoleBuilder.getConsole)
    consoles.put(toolKey, c)
    toolsCombo.addItem(toolKey)
    c
  }

  /** Get the currently selected tool. */
  private def selectedToolKey(): Option[ToolKey] = Option(toolsCombo.getSelectedItem).map {
    case t: ToolKey => t
    case other => throw new RuntimeException(s"Expected ToolKey, got: $other")
  }
}

final case class ToolConsoleView private(view: ConsoleView) {

  def getComponent: JComponent = mainPanel

  private val mainPanel = new SimpleToolWindowPanel(false, true)

  private def initUI() = {
    mainPanel.setContent(view.getComponent)
    mainPanel.setToolbar(createToolbar())
    this
  }

  private def createToolbar() = {
    val toolbar = new JPanel(new GridLayout())
    val group = new DefaultActionGroup()
    // Default console toolbar actions, e.g. previous, next, toggle soft wraps, scroll to end
    view.createConsoleActions().foreach { action => group.add(action) }
    val actionToolbar = ActionManager.getInstance().createActionToolbar(
      "HaskellToolsConsoleToolbar",
      group,
      false
    )
    toolbar.add(actionToolbar.getComponent)
    toolbar
  }
}

object ToolConsoleView {
  def create(view: ConsoleView): ToolConsoleView = ToolConsoleView(view).initUI()
}

object HaskellToolsConsole {

  val logger = Logger.getInstance(classOf[HaskellToolsConsole])

  /** Gets the HaskellToolsConsole for the given Project. */
  def get(project: Project): HaskellToolsConsole = {
    consoles.getOrElse(project, create(project))
  }

  def dispose(console: HaskellToolsConsole): Unit = {
    console.consoles.foreach { case (_, view) => view.view.dispose() }
  }

  /** Creates a new HaskellToolsConsole, storing the result in the consoles cache. */
  private def create(project: Project): HaskellToolsConsole = {
    val console = new HaskellToolsConsole(project)
    consoles.put(project, console)
    console
  }

  private val consoles = mutable.Map[Project, HaskellToolsConsole]()

  final class Curried(val console: HaskellToolsConsole, toolKey: ToolKey) {
    def writeInput(msg: String): Unit = console.writeInput(toolKey, msg)
    def writeOutput(msg: String): Unit = console.writeOutput(toolKey, msg)
    def writeError(msg: String): Unit = console.writeError(toolKey, msg)
  }
}

/** Factory which lazily creates the Haskell Tools Console ToolWindow on demand. */
class HaskellToolsConsoleWindowFactory extends ToolWindowFactory with Condition[Project] {

  /** Lazily creates the tool window when opened by the user. */
  override def createToolWindowContent(project: Project, toolWindow: ToolWindow): Unit = {
    val console = HaskellToolsConsole.get(project)
    console.initUI()
    val content = ContentFactory.SERVICE.getInstance().createContent(console.component, "", false)
    toolWindow.getContentManager.addContent(content)
    // Explicitly dispose resources.
    // See https://github.com/carymrobbins/intellij-haskforce/issues/269
    content.setDisposer(new Disposable {
      override def dispose(): Unit = HaskellToolsConsole.dispose(console)
    })
  }

  /**
   * Implemented for Condition[Project] to specify if we should display this tool window.
   * Only display this tool window if our project has at least one Haskell module.
   */
  override def value(project: Project): Boolean = {
    !ModuleUtil.getModulesOfType(project, HaskellModuleType.getInstance).isEmpty
  }
}
