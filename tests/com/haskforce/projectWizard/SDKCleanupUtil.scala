package com.haskforce.projectWizard

import com.haskforce.HaskellSdkType
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil

object SDKCleanupUtil {

  /**
    * As of 2017.3, the IntelliJ test framework has a strict policy about "leaking"
    * libraries and SDKs; as such, we need to call this in tests' tearDown methods.
    * Otherwise, we will get `java.lang.AssertionError: Leaked SDKs` during tests which
    * create an SDK.
    *
    * See: https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000648324-Virtual-pointer-hasn-t-been-disposed-on-LATEST-EAP-SNAPSHOT
    */
  def cleanupHaskellSDK(): Unit = {
    ApplicationManager.getApplication.runWriteAction({ () =>
      SdkConfigurationUtil.removeSdk(HaskellSdkType.findOrCreateSdk())
    }: Runnable)
  }
}
