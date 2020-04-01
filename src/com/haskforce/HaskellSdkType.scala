package com.haskforce

import java.util.Comparator

import com.haskforce.jps.model.JpsHaskellModelSerializerExtension
import com.intellij.openapi.projectRoots._
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import javax.swing._
import org.jdom.Element
import org.jetbrains.annotations.{NotNull, Nullable}

/**
 * Responsible for the mechanics when pressing "+" in the SDK configuration,
 * as well as the project SDK configuration.
 */
object HaskellSdkType {
  /**
   * Returns the Haskell SDK.
   */
  @NotNull def getInstance: HaskellSdkType = {
    SdkType.findInstance(classOf[HaskellSdkType])
  }

  def findOrCreateSdk(): Sdk = {
    val sdkType = getInstance
    // Essentially, sorts the Sdks so that the Haskell one comes first.
    // If it doesn't exist, should create a new one.
    val cmp: Comparator[Sdk] = { (sdk1: Sdk, sdk2: Sdk) =>
      if (sdk1.getSdkType == sdkType) -1
      else if (sdk2.getSdkType == sdkType) 1
      else 0
    }
    SdkConfigurationUtil.findOrCreateSdk(cmp, sdkType)
  }
}

class HaskellSdkType extends SdkType(JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID) {
  /**
   * Returns the icon to be used for Haskell things in general.
   */
  override def getIcon: Icon = HaskellIcons.FILE

  @Nullable def createAdditionalDataConfigurable
      (sdkModel: SdkModel, sdkModificator: SdkModificator)
      : AdditionalDataConfigurable = null

  def getPresentableName: String = {
    JpsHaskellModelSerializerExtension.HASKELL_SDK_TYPE_ID
  }

  /**
   * Currently a no-op.
   */
  def saveAdditionalData(@NotNull additionalData: SdkAdditionalData, @NotNull additional: Element) {
  }

  /**
   * Approves of a path as SDK home.
   * Currently we allow anything since the user configures their tools.
   */
  def isValidSdkHome(path: String): Boolean = true

  def suggestSdkName(currentSdkName: String, sdkHome: String): String = "Haskell"

  @Nullable override def getVersionString(sdkHome: String): String = null

  /**
   * We don't need to suggest the SDK path for the same reason as isValidSdkHome.
   * Don't return null so this will still be interpreted as a "valid" SDK by
   * other utilities (e.g. StackProjectImportBuilder).
   */
  @Nullable def suggestHomePath: String = ""

  /**
   * We can modify the SDK using sdk.getSdkModificator, but for now we don't need to.
   */
  override def setupSdkPaths(sdk: Sdk, sdkModel: SdkModel): Boolean = true
}
