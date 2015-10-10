package com.haskforce.cabal.settings.ui

import javax.swing._

import com.haskforce.cabal.settings.CabalComponentType
import com.haskforce.ui.SComboBox

/**
 * Simple trait to share code via AddCabalPackageUtil between various forms
 * which help create Cabal packages.
 */
trait AddCabalPackageForm {
  def getCabalVersionField: JTextField
  def getAuthorField: JTextField
  def getLicenseField: SComboBox[String]
  def getEmailField: JTextField
  def getHomepageField: JTextField
  def getSynopsisField: JTextField
  def getCategoryField: SComboBox[String]
  def getComponentTypeField: SComboBox[CabalComponentType]
  def getLanguageField: SComboBox[String]
  def getSourceDirField: JTextField
  def getPackageVersionField: JTextField
  def getGenerateCommentsField: JCheckBox
}
