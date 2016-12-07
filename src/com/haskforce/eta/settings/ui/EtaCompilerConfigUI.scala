package com.haskforce.eta.settings.ui

import java.awt.{Color, GridBagLayout}
import javax.swing._

import com.haskforce.eta.jps.model.EtaBuildOptions
import com.haskforce.ui.GC
import com.haskforce.utils.GuiUtil
import com.intellij.openapi.ui.TextFieldWithBrowseButton

/**
 * Interface for creating and interacting with the Eta compiler configuration UI.
 * See the implementation in [[EtaCompilerConfigUIImpl]].
 * The implementation takes care of building the appropriate UI elements and
 * exposing field data. This allows us to separate the UI logic from configuration logic.
 */
trait EtaCompilerConfigUI {
  def getComponent: JComponent
  def getState: EtaBuildOptions
  def setState(state: EtaBuildOptions): Unit
}

object EtaCompilerConfigUI {
  def create(): EtaCompilerConfigUI = new EtaCompilerConfigUIImpl
}

private class EtaCompilerConfigUIImpl extends EtaCompilerConfigUI {

  override def getState: EtaBuildOptions = {
    val state = new EtaBuildOptions
    state.etaPath = etaPath.getText
    state.etaPkgPath = etaPkgPath.getText
    state.etlasPath = etlasPath.getText
    state
  }

  override def setState(state: EtaBuildOptions): Unit = {
    etaPath.setText(state.etaPath)
    etaPkgPath.setText(state.etaPkgPath)
    etlasPath.setText(state.etlasPath)
  }

  override val getComponent = new JPanel(new GridBagLayout) {
    // Default grid constraints
    val gc = GC.pad(10, 5).northWest

    // Start at y position 0 and increment for each vertical element.
    var gridY = 0

    private def addExeField(e: ExeField) = {
      add(e.label, gc.grid(0, gridY))
      add(e, gc.fillHorizontal.grid(1, gridY))
      gridY += 1
      add(e.errorsField, gc.fillHorizontal.grid(1, gridY))
    }

    addExeField(etaPath)

    gridY += 1
    addExeField(etaPkgPath)

    gridY += 1
    addExeField(etlasPath)
  }

  private lazy val etaPath = new ExeField("Eta Path:", "eta")
  private lazy val etaPkgPath = new ExeField("Eta-pkg Path:", "eta-pkg")
  private lazy val etlasPath = new ExeField("Etlas Path:", "etlas")

  sealed class ExeField(labelText: String, exe: String) extends TextFieldWithBrowseButton {

    lazy val label = new JLabel(labelText)

    lazy val errorsField = new JLabel
    errorsField.setForeground(Color.red)

    GuiUtil.addFolderListener(this, exe)
  }
}
