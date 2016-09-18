package com.haskforce.tools.cabal.settings.ui

import java.awt.event.{KeyEvent, ActionEvent}
import java.awt.{Dimension, GridBagLayout}
import javax.swing._

import com.haskforce.Implicits._
import com.haskforce.ui.GC
import com.haskforce.utils.FileUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.uiDesigner.core.Spacer

/**
 * Dialog for importing Cabal packages into IntelliJ modules.
 */
class DiscoverCabalPackagesDialog(
  project: Project,
  cabalFiles: Seq[VirtualFile],
  callback: Seq[VirtualFile] => Unit
) extends JDialog {
  private val contentPane = new JPanel(new GridBagLayout)
  private val buttonImport = new JButton("Import")
  private val buttonCancel = new JButton("Cancel")
  private val fieldMap = cabalFiles.map(_ -> new JCheckBox("")).sortBy(_._1.getName.toLowerCase)
  private val checkBoxImportAll = new JCheckBox("")

  setupUI()

  private def setupUI(): Unit = {
    setContentPane(contentPane)
    // The Import button should be enabled when one of the checkboxes are selected.
    buttonImport.setEnabled(false)
    setLocationRelativeTo(null)
    setupComponents()
    setupListeners()
    pack()
    setModal(true)
    setResizable(false)
    setVisible(true)
  }

  private def getPackagePath(file: VirtualFile): String = {
    FileUtil.toRelativePath(project, file.getParent)
  }

  private def setupListeners(): Unit = {
    buttonImport.addActionListener { e: ActionEvent => onImport() }
    buttonCancel.addActionListener { e: ActionEvent => onCancel() }
    fieldMap.foreach { case (_, checkBox) => checkBox.addActionListener { e: ActionEvent => onCheck() } }
    checkBoxImportAll.addActionListener { e: ActionEvent => onCheckImportAll() }
    // Cancel when user presses Escape.
    contentPane.registerKeyboardAction(
      { e: ActionEvent => onCancel() },
      KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
    )
  }

  private def getCabalFilesToImport: Seq[VirtualFile] = fieldMap.collect {
    case (file, shouldImport) if shouldImport.isSelected => file
  }

  /**
   * Execute callback when Import is clicked.
   */
  private def onImport(): Unit = {
    callback(getCabalFilesToImport)
    dispose()
  }

  private def onCancel(): Unit = {
    dispose()
  }

  /**
   * Only enable the OK button when at least one checkbox is selected.
   */
  private def onCheck(): Unit = {
    val anyChecked = fieldMap.exists { case (_, checkBox) => checkBox.isSelected }
    buttonImport.setEnabled(anyChecked)
  }

  /**
   * Toggle all import checkboxes.
   */
  private def onCheckImportAll(): Unit = {
    val isSelected = checkBoxImportAll.isSelected
    fieldMap.foreach { case (_, checkBox) => checkBox.setSelected(isSelected) }
    // Enable/disable the Import button.
    onCheck()
  }

  private def setupComponents(): Unit = {
    val gc = GC.weight(x = 0.5).pad(10, 5).northWest

    val listPanel = new JPanel(new GridBagLayout) {
      add(checkBoxImportAll, gc.grid(0, 0))
      add(new JLabel("<html><b>Package</b></html>"), gc.grid(1, 0))
      add(new JLabel("<html><b>Directory</b></html>"), gc.grid(2, 0))
      add(new JSeparator(SwingConstants.HORIZONTAL), gc.fillHorizontal.width(3).grid(0, 1))

      fieldMap.zipWithIndex.foreach { case ((file, shouldImport), i) =>
        val y = i + 2
        add(shouldImport, gc.grid(0, y))
        add(new JLabel(file.getNameWithoutExtension), gc.grid(1, y))
        add(new JLabel(getPackagePath(file)), gc.grid(2, y))
      }
      add(new Spacer, gc.weight(y = 1).grid(0, fieldMap.length + 2))
    }
    val scrollPane = new JScrollPane(listPanel)
    scrollPane.setPreferredSize(new Dimension(800, getListPanelHeight))
    getContentPane.add(scrollPane, gc.fillBoth.grid(0, 0))

    val buttonPanel = new JPanel(new GridBagLayout) {
      add(buttonImport, gc.grid(0, 0))
      add(buttonCancel, gc.grid(1, 0))
    }
    getContentPane.add(buttonPanel, gc.lastLineEnd.grid(0, 1))

    validate()
  }

  private def getListPanelHeight: Int = Math.min((fieldMap.length + 1) * 30, 450)
}
