package com.haskforce;

import com.haskforce.core.HaskForceCoreMeta;
import com.haskforce.utils.NotificationUtil;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.components.ApplicationComponent;

public class HaskForcePluginVerifier implements ApplicationComponent {

  @Override
  public void initComponent() {
    String expectedCoreVersion = HaskForceMeta.CORE_VERSION;
    String actualCoreVersion = HaskForceCoreMeta.VERSION;
    if (!expectedCoreVersion.equals(actualCoreVersion)) {
      NotificationUtil.displaySimpleNotification(
        NotificationType.WARNING,
        null,
        "Haskell plugin compatibility",
        "HaskForce depends on the haskforce-core library version " + expectedCoreVersion
          + "; however, version " + actualCoreVersion + " was found. Another Haskell plugin "
          + "may be overriding this library. Functionality may not work as expected."
      );
    }
  }
}
