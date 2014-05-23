/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.haskforce.utils;

import com.haskforce.settings.HaskellToolsConfigurable;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationListener;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import javax.swing.event.HyperlinkEvent;

/**
 * Object sent over the notification bus.
 */
public class HaskellToolsNotificationListener implements NotificationListener {
    @NotNull
    private final Project myProject;

    public HaskellToolsNotificationListener(@NotNull Project project) {
        myProject = project;
    }

    /**
     * Shows the settings dialog when the user presses "configure" on a balloon.
     */
    @Override
    public void hyperlinkUpdate(@NotNull Notification notification, @NotNull HyperlinkEvent event) {
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            if (event.getDescription().equals("configureHaskellTools") && !myProject.isDisposed()) {
                ShowSettingsUtil.getInstance().showSettingsDialog(myProject, HaskellToolsConfigurable.HASKELL_TOOLS_ID);
                notification.expire();
            }
        }
    }
}
