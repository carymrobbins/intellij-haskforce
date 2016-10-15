package com.haskforce.tools.ghcmod.modi.actions;

import com.haskforce.system.packages.HPackageModule;
import com.haskforce.system.settings.ToolKey;
import com.haskforce.tools.ghcmod.modi.GhcModi;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.ui.components.JBList;
import org.jetbrains.annotations.NotNull;
import scala.collection.JavaConversions;

import javax.swing.*;
import java.util.Collection;

public class RestartGhcModi extends AnAction implements DumbAware {
    private static final Logger LOG = Logger.getInstance(RestartGhcModi.class);

    public static final String MENU_PATH = "Tools > Restart ghc-modi";

    @Override
    public void update(@NotNull AnActionEvent e) {
        e.getPresentation().setEnabled(enabled(e));
    }

    private static boolean enabled(@NotNull AnActionEvent e) {
        final Project project = getEventProject(e);
        if (project == null) return false;
        final String ghcModiPath = ToolKey.GHC_MODI_KEY.getPath(project);
        boolean existsActiveModule = HPackageModule.getModulesWithPackage(project).nonEmpty();
        return ghcModiPath != null && !ghcModiPath.isEmpty() && existsActiveModule;
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        final String prefix = "Unable to restart ghc-modi - ";
        Project project = e.getProject();
        if (project == null) { displayError(e, prefix + "No active project."); return; }
        Collection<Module> modules = JavaConversions.asJavaCollection(HPackageModule.getModulesWithPackage(project));
        int size = modules.size();
        if (size == 0) displayError(e, prefix + "No Haskell modules are used in this project.");
        else if (size == 1) restartGhcModi(e, modules.iterator().next());
        else showModuleChoicePopup(e, project, modules);
    }

    private static void showModuleChoicePopup(@NotNull AnActionEvent e, Project project, Collection<Module> modules) {
        final JList list = new JBList(JBList.createDefaultListModel(modules.toArray()));
        JBPopup popup = JBPopupFactory.getInstance()
                .createListPopupBuilder(list)
                .setTitle("Restart ghc-modi for module")
                .setItemChoosenCallback(makeModuleChoiceCallback(e, list))
                .createPopup();
        popup.showCenteredInCurrentWindow(project);
    }

    private static Runnable makeModuleChoiceCallback(final @NotNull AnActionEvent e, final @NotNull JList list) {
        return new Runnable() {
            @Override
            public void run() {
                restartGhcModi(e, (Module)list.getSelectedValue());
            }
        };
    }

    private static void restartGhcModi(@NotNull AnActionEvent e, @NotNull Module module) {
        GhcModi ghcModi = module.getComponent(GhcModi.class);
        if (ghcModi == null) displayError(e, "Could not find module component for ghc-modi.");
        else ghcModi.restart();
    }

    private static void displayError(@NotNull AnActionEvent e, @NotNull String message) {
        final String groupId = e.getPresentation().getText();
        Notifications.Bus.notify(new Notification(
                groupId, "Restart ghc-modi", message, NotificationType.ERROR), getEventProject(e));
        LOG.warn(message);
    }
}
