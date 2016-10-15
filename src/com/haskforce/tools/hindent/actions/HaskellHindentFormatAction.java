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

package com.haskforce.tools.hindent.actions;

/*
 * Adapted from the Stylish Haskell formatter
 */

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import org.apache.sanselan.util.IOUtils;
import org.jetbrains.annotations.NotNull;

import com.haskforce.haskell.psi.HaskellFile;
import com.haskforce.system.settings.ToolKey;
import com.haskforce.system.utils.HaskellToolsNotificationListener;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessAdapter;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.ExceptionUtil;

/**
 * Action that calls hindent on the buffer it is invoked for.
 */
public class HaskellHindentFormatAction extends AnAction implements DumbAware {
    private static final String NOTIFICATION_TITLE = "Reformat code with Hindent";
    private static final Logger LOG = Logger.getInstance(HaskellHindentFormatAction.class);

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
     * Main entry point. Calls hindent and detects errors.
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
            final String hindentPath = ToolKey.HINDENT_KEY.getPath(project);
            final String hindentFlags = ToolKey.HINDENT_KEY.getFlags(project);
            if (hindentPath == null || hindentPath.isEmpty()) {
                Notifications.Bus.notify(
                        new Notification(groupId, NOTIFICATION_TITLE,
                                "Hindent executable path is empty"+
                                        "<br/><a href='configureHaskellTools'>Configure</a>",
                                NotificationType.WARNING, new HaskellToolsNotificationListener(project)), project);
                return;
            }
            commandLine.setExePath(hindentPath);
            commandLine.getParametersList().addParametersString(hindentFlags);

            final VirtualFile backingFile = psiFile.getVirtualFile();
            if (backingFile == null) return;

            ApplicationManager.getApplication().saveAll();

            final String commandLineString = commandLine.getCommandLineString();
            OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLineString);

            // Pipe file into the process
            final InputStream fileContents = backingFile.getInputStream();
            final OutputStream processStdin = handler.getProcessInput();
            IOUtils.copyStreamToStream(fileContents, processStdin);

            handler.addProcessListener(new CapturingProcessAdapter() {
                @Override
                public void processTerminated(@NotNull final ProcessEvent event) {
                    List<String> errorDetection = getOutput().getStderrLines();
                    if (!errorDetection.isEmpty()) {
                        String firstLine = errorDetection.get(0);
                        if (firstLine.startsWith("hindent:")) {
                            // Filter out the left part and keep the interesting stuff.
                            // Error message is on the format:
                            // hindent: interesting stuff.
                            String output = firstLine.split(": ", 2)[1];
                            Notifications.Bus.notify(new Notification(groupId,
                                    "Hindent error.", output,
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
                                        psiFile.getName() + " formatted with Hindent.",
                                        NotificationType.INFORMATION), project);

                            } catch (Exception ex) {
                                Notifications.Bus.notify(new Notification(groupId,
                                        "Formatting " + psiFile.getName() + "  with Hindent failed.", ExceptionUtil.getUserStackTrace(ex, LOG),
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
                    "Formatting " + psiFile.getName() + " with Hindent failed", ExceptionUtil.getUserStackTrace(ex, LOG),
                    NotificationType.ERROR), project);
            LOG.error(ex);
        }
    }
}
