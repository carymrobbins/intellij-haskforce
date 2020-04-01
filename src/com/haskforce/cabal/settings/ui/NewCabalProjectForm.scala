package com.haskforce.cabal.settings.ui

import java.awt.GridBagLayout
import java.awt.event.ActionEvent

import com.haskforce.cabal.settings.{CabalComponentType, CabalFileData}
import com.haskforce.ui.{GC, SComboBox}
import com.intellij.uiDesigner.core.Spacer
import javax.swing._

/** Form presented to user to configure Cabal initialization when creating a new project */
class NewCabalProjectForm {

  val initializeCabalPackageField = new JCheckBox("Initialize Cabal package", true)
  val packageVersionField = new JTextField("0.1.0.0")
  val synopsisField = new JTextField()
  val homepageField = new JTextField()
  val authorNameField: JTextField = AddCabalPackageUtil.newAuthorField()
  val maintainerEmailField: JTextField = AddCabalPackageUtil.newEmailField()
  val categoryField: SComboBox[String] = AddCabalPackageUtil.newCategoryField()
  val cabalVersionField = new JTextField(">=1.10")
  val componentTypeField: SComboBox[CabalComponentType] = AddCabalPackageUtil.newComponentTypeField()
  val sourceDirField = new JTextField("src")
  val languageField: SComboBox[String] = AddCabalPackageUtil.newLanguageField()

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

  private val contentPane: JPanel = new JPanel(new GridBagLayout) {
    private val gc = GC.default.pad(10, 5).northWest

    add(initializeCabalPackageField, gc.width(2).weight(x = 1).grid(0, 0))
    private val nextY = addFieldList(y = 1)
    add(new Spacer, gc.grid(0, nextY).weight(y = 1))

    //noinspection SameParameterValue
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

  def getContentPane: JPanel = contentPane

  def shouldInitializeCabalPackage: Boolean = initializeCabalPackageField.isSelected

  def getData: CabalFileData = CabalFileData(
    packageVersion = packageVersionField.getText,
    synopsis = synopsisField.getText,
    homepage = homepageField.getText,
    author = authorNameField.getText,
    maintainer = maintainerEmailField.getText,
    category = categoryField.getSelectedItem.toString,
    cabalVersion = cabalVersionField.getText,
    // The .asInstanceOf should be ok since the user can only pick a CabalComponentType.
    componentType = componentTypeField.getSelectedItem.asInstanceOf[CabalComponentType],
    sourceDir = sourceDirField.getText,
    language = languageField.getSelectedItem.toString
  )
}
