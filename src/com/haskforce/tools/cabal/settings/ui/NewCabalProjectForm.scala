package com.haskforce.tools.cabal.settings.ui

import java.awt.GridBagLayout
import java.awt.event.ActionEvent
import javax.swing._

import com.intellij.uiDesigner.core.Spacer

import com.haskforce.Implicits._
import com.haskforce.ui.GC

/**
 * Form presented to user to configure Cabal initialization when creating a new project.
 */
class NewCabalProjectForm {
  val initializeCabalPackageField = new JCheckBox("Initialize Cabal package", true)
  val packageVersionField = new JTextField("0.1.0.0")
  val synopsisField = new JTextField()
  val homepageField = new JTextField()
  val authorNameField = AddCabalPackageUtil.newAuthorField()
  val maintainerEmailField = AddCabalPackageUtil.newEmailField()
  val categoryField = AddCabalPackageUtil.newCategoryField()
  val cabalVersionField = new JTextField(">=1.10")
  val componentTypeField = AddCabalPackageUtil.newComponentTypeField()
  val sourceDirField = new JTextField("src")
  val languageField = AddCabalPackageUtil.newLanguageField()

  val fieldMap = Seq(
    "Version:" -> packageVersionField,
    "Synopsis:" -> synopsisField,
    "Homepage:" -> homepageField,
    "Author Name:" -> authorNameField,
    "Maintainer Email:" -> maintainerEmailField,
    "Category:" -> categoryField,
    "Cabal Version:" -> cabalVersionField,
    "Component Type:" -> componentTypeField,
    "Source Directory:" -> sourceDirField,
    "Language:" -> languageField
  )

  private val contentPane = new JPanel(new GridBagLayout) {
    val gc = GC.pad(10, 5).northWest

    add(initializeCabalPackageField, gc.width(2).weight(x = 1).grid(0, 0))
    val nextY = addFieldList(y = 1)
    add(new Spacer, gc.grid(0, nextY).weight(y = 1))

    private def addFieldList(y: Int): Int = {
      fieldMap.zipWithIndex.foreach { case ((name, field), i) =>
        add(new JLabel(name), gc.grid(0, y + i))
        add(field, gc.fillHorizontal.grid(1, y + i))
      }
      y + fieldMap.length
    }
  }

  // Toggle field enabling
  initializeCabalPackageField.addActionListener { e: ActionEvent =>
    fieldMap.foreach(_._2.setEnabled(shouldInitializeCabalPackage))
  }

  def getContentPane = contentPane
  def shouldInitializeCabalPackage: Boolean = initializeCabalPackageField.isSelected
}
