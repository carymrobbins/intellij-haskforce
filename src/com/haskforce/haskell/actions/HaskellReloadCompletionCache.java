package com.haskforce.haskell.actions;

import com.haskforce.haskell.codeInsight.HaskellCompletionCacheLoader;
import com.haskforce.haskell.psi.HaskellFile;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiFile;

public class HaskellReloadCompletionCache extends AnAction implements DumbAware {
    private static final Logger LOG = Logger.getInstance(HaskellStylishFormatAction.class);

    @Override
    public void update(AnActionEvent e) {
        final PsiFile file = e.getData(CommonDataKeys.PSI_FILE);
        e.getPresentation().setEnabled(file != null && file instanceof HaskellFile);
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        final PsiFile file = e.getData(CommonDataKeys.PSI_FILE);
        if (file == null) {
            final String groupId = e.getPresentation().getText();
            final String message = "Unexpected error: could not identify file to reload completion cache!";
            Notifications.Bus.notify(new Notification(
                    groupId, "Reload Completion Cache", message, NotificationType.ERROR), getEventProject(e));
            LOG.error(message);
            return;
        }
        HaskellCompletionCacheLoader.get(file.getProject()).forceUpdateCache(file);
    }
}
