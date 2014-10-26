package com.haskforce.actions;

import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.psi.HaskellFile;
import com.haskforce.utils.ExecUtil;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
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
        final Project project = file.getProject();
        final String workDir = ExecUtil.guessWorkDir(file);
        HaskellCompletionContributor.loadCacheData(file, project, workDir, true);
    }
}
