package com.haskforce.utils;

import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

public class NotificationUtil {
    public static void displayToolsNotification(@NotNull NotificationType type,
                                                @NotNull Project project,
                                                @NotNull String title,
                                                @NotNull String message) {
        Notifications.Bus.notify(new Notification(
                title, title,
                message.replace("\n", "<br/>") + "<br/><a href='configureHaskellTools'>Configure</a>",
                type, new HaskellToolsNotificationListener(project)), project);
    }
}
