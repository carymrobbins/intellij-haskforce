package com.haskforce.cabal.settings.ui

import java.awt.GridBagLayout
import java.awt.event.ActionEvent
import javax.swing._

import com.haskforce.Implicits._
import com.haskforce.cabal.settings.CabalBuildType
import com.haskforce.ui.{GC, SComboBox}

/**
 * Form presented to user to configure Cabal initialization when creating a new project.
 */
class NewCabalProjectForm extends AddCabalPackageForm {
  val getInitializeCabalPackage = new JCheckBox("Initialize Cabal package", true)
  override val getCabalVersionField = new JTextField()
  override val getSynopsisField = new JTextField()
  override val getGenerateCommentsField = new JCheckBox("Generate explanatory comments")
  override val getBuildTypeField = new SComboBox[CabalBuildType]
  override val getCategoryField = new SComboBox[String]
  override val getSourceDirField = new JTextField()
  override val getHomepageField = new JTextField()
  override val getAuthorField = new JTextField()
  override val getEmailField = new JTextField()
  override val getLicenseField = new SComboBox[String]
  override val getPackageVersionField = new JTextField()
  override val getLanguageField = new SComboBox[String]

  private val contentPane = new JPanel(new GridBagLayout) {
    val gc = GC.pad(10, 5).northWest

    add(getInitializeCabalPackage, gc.width(2).weight(x = 1).grid(0, 0))
    val nextY = addFieldList(y = 1)
    add(getGenerateCommentsField, gc.width(2).weight(1, 1).grid(0, nextY))
//    add(new Spacer, gc.weight(y = 1))

    def addFieldList(y: Int): Int = {
      val fieldMap = Seq(
        "Version:" -> getPackageVersionField,
        "Build Type:" -> getBuildTypeField,
        "Source Directory:" -> getSourceDirField,
        "Cabal Version:" -> getCabalVersionField,
        "License:" -> getLicenseField,
        "Author:" -> getAuthorField,
        "Email:" -> getEmailField,
        "Homepage:" -> getHomepageField,
        "Synopsis:" -> getSynopsisField,
        "Category:" -> getCategoryField,
        "Language:" -> getLanguageField
      )
      fieldMap.zipWithIndex.foreach { case ((name, field), i) =>
        add(new JLabel(name), gc.grid(0, y + i))
        add(field, gc.fillHorizontal.grid(1, y + i))
      }
      y + fieldMap.length
    }
  }

  AddCabalPackageUtil.setupFields(None, this)

  // Toggle field enabling
  getInitializeCabalPackage.addActionListener { e: ActionEvent =>
    Seq(
      getCabalVersionField,
      getSynopsisField,
      getGenerateCommentsField,
      getBuildTypeField,
      getCategoryField,
      getSourceDirField,
      getHomepageField,
      getAuthorField,
      getEmailField,
      getLicenseField,
      getPackageVersionField,
      getLanguageField
    ).foreach(_.setEnabled(shouldInitializeCabalPackage))
  }

  def getContentPane = contentPane
  def shouldInitializeCabalPackage = getInitializeCabalPackage.isSelected
}
