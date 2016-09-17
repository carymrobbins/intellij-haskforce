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

package com.haskforce.haskell.actions;

/*
 * Adapted from the Emacs-formatter in the Erlang plugin. 17 May 2014.
 */

import com.haskforce.settings.ToolKey;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiFile;
import com.haskforce.haskell.psi.HaskellFile;
import com.haskforce.utils.HaskellToolsNotificationListener;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessAdapter;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.util.ExceptionUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Action that calls stylish-haskell on the buffer it is invoked for.
 */
public class HaskellStylishFormatAction extends AnAction implements DumbAware {
    private static final String NOTIFICATION_TITLE = "Reformat code with Stylish-Haskell";
    private static final Logger LOG = Logger.getInstance(HaskellStylishFormatAction.class);

    /**
     * Enable the action for Haskell files.
     */
    @Override
    public void update(AnActionEvent e) {
        PsiFile psiFile = e.getData(CommonDataKeys.PSI_FILE);
        boolean isHaskell = psiFile instanceof HaskellFile;
        e.getPresentation().setEnabled(isHaskell);
    }

    /**
     * Main entry point. Calls stylish-haskell and detects errors.
     */
    @Override
    public void actionPerformed(AnActionEvent e) {
        final PsiFile psiFile = e.getData(CommonDataKeys.PSI_FILE);
        final Project project = getEventProject(e);
        if (project == null) return;
        if (!(psiFile instanceof HaskellFile)) return;
        VirtualFile virtualFile = psiFile.getVirtualFile();
        if (virtualFile == null) return;

        final String groupId = e.getPresentation().getText();
        try {
            GeneralCommandLine commandLine = new GeneralCommandLine();
            final String stylishPath = ToolKey.STYLISH_HASKELL_KEY.getPath(project);
            final String stylishFlags = ToolKey.STYLISH_HASKELL_KEY.getFlags(project);
            if (stylishPath == null || stylishPath.isEmpty()) {
                Notifications.Bus.notify(
                        new Notification(groupId, NOTIFICATION_TITLE,
                                "Stylish-Haskell executable path is empty"+
                                        "<br/><a href='configureHaskellTools'>Configure</a>",
                                NotificationType.WARNING, new HaskellToolsNotificationListener(project)), project);
                return;
            }
            commandLine.setExePath(stylishPath);
            commandLine.getParametersList().addParametersString(stylishFlags);

            final VirtualFile backingFile = psiFile.getVirtualFile();
            if (backingFile == null) return;
            String backingFilePath = backingFile.getCanonicalPath();
            if (backingFilePath == null) return;
            commandLine.addParameter(backingFilePath);
            // Set the work dir so stylish can pick up the user config, if it exists.
            commandLine.setWorkDirectory(backingFile.getParent().getCanonicalPath());

            ApplicationManager.getApplication().saveAll();

            final String commandLineString = commandLine.getCommandLineString();
            OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLineString);
            handler.addProcessListener(new CapturingProcessAdapter() {
                @Override
                public void processTerminated(@NotNull final ProcessEvent event) {
                    List<String> errorDetection = getOutput().getStderrLines();
                    if (!errorDetection.isEmpty()) {
                        String firstLine = errorDetection.get(0);
                        if (firstLine.startsWith("Language.Haskell.Stylish.Parse.parseModule:")) {
                            // Filter out the left part and keep the interesting stuff.
                            // Error message is on the format:
                            // moduleName: interesting stuff.
                            String output = firstLine.split(":", 2)[1];
                            Notifications.Bus.notify(new Notification(groupId,
                                    "Stylish-Haskell error.", output,
                                    NotificationType.ERROR), project);
                            return;
                        }
                        return;
                    }
                  final String text = getOutput().getStdout();
                    ApplicationManager.getApplication().invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                final Document document = PsiDocumentManager.getInstance(project).getDocument(psiFile);
                                if (document == null) return;
                                CommandProcessor.getInstance().executeCommand(project, new Runnable() {
                                    @Override
                                    public void run() {
                                        ApplicationManager.getApplication().runWriteAction(new Runnable() {
                                            @Override
                                            public void run() {
                                                document.setText(text);
                                            }
                                        });
                                    }
                                }, NOTIFICATION_TITLE, "", document);

                                Notifications.Bus.notify(new Notification(groupId, NOTIFICATION_TITLE,
                                        psiFile.getName() + " formatted with Stylish-Haskell.",
                                        NotificationType.INFORMATION), project);

                            } catch (Exception ex) {
                                Notifications.Bus.notify(new Notification(groupId,
                                        "Formatting " + psiFile.getName() + "  with Stylish-Haskell failed.", ExceptionUtil.getUserStackTrace(ex, LOG),
                                        NotificationType.ERROR), project);
                                LOG.error(ex);
                            }
                        }
                    });
                }
            });
            handler.startNotify();
        } catch (Exception ex) {
            Notifications.Bus.notify(new Notification(groupId,
                    "Formatting " + psiFile.getName() + " with Stylish-Haskell failed", ExceptionUtil.getUserStackTrace(ex, LOG),
                    NotificationType.ERROR), project);
            LOG.error(ex);
        }
    }
}
