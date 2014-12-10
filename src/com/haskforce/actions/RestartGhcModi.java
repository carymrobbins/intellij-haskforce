package com.haskforce.actions;

import com.haskforce.highlighting.annotation.external.GhcModi;
import com.haskforce.settings.ToolKey;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class RestartGhcModi extends AnAction implements DumbAware {
    private static final Logger LOG = Logger.getInstance(HaskellStylishFormatAction.class);

    public static final String MENU_PATH = "Tools > Restart ghc-modi";

    @Override
    public void update(AnActionEvent e) {
        e.getPresentation().setEnabled(enabled(e));
    }

    private static boolean enabled(AnActionEvent e) {
        final Project project = getEventProject(e);
        if (project == null) { return false; }
        final String ghcModiPath = ToolKey.GHC_MODI_KEY.getPath(project);
        return ghcModiPath != null && !ghcModiPath.isEmpty();
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        final PsiFile file = e.getData(CommonDataKeys.PSI_FILE);
        if (file == null) { displayError(e, "Please open a Haskell file before restarting ghc-modi."); return; }
        final Module module = ModuleUtilCore.findModuleForPsiElement(file);
        if (module == null) { displayError(e, "Could not find IntelliJ module for current file."); return; }
        GhcModi ghcModi = module.getComponent(GhcModi.class);
        if (ghcModi == null) { displayError(e, "Could not find module component for ghc-modi."); return; }
        final String canonicalPath = file.getVirtualFile().getCanonicalPath();
        if (canonicalPath == null) { displayError(e, "Could not find canonical path for current file."); return; }
        ghcModi.restartAndCheck(canonicalPath);
    }

    private static void displayError(AnActionEvent e, @NotNull String message) {
        final String groupId = e.getPresentation().getText();
        Notifications.Bus.notify(new Notification(
                groupId, "Restart ghc-modi", message, NotificationType.ERROR), getEventProject(e));
        LOG.warn(message);
    }
}
