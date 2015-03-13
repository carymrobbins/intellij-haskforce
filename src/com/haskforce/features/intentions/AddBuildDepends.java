package com.haskforce.features.intentions;


import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.haskforce.cabal.index.CabalFileIndex;
import com.haskforce.cabal.psi.*;
import com.haskforce.cabal.psi.impl.CabalElementFactory;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupChooserBuilder;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.ScrollPaneFactory;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBPanel;
import com.intellij.util.IncorrectOperationException;
import org.jdesktop.swingx.combobox.ListComboBoxModel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

public class AddBuildDepends extends BaseIntentionAction {

    public final String packageName;

    public AddBuildDepends(String packageName) {
        this.packageName = packageName;
    }

    @NotNull
    @Override
    public String getFamilyName() {
        return "Add build depends";
    }

    @NotNull
    @Override
    public String getText() {
        return "Add " + packageName + " to build depends";
    }


    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile psiFile) {
        return true;
    }

    @Override
    public void invoke(final @NotNull Project project, final Editor editor, PsiFile psiFile) throws IncorrectOperationException {
        CabalFile cabalFile = CabalFileIndex.getCabalFileByProjectName(project, GlobalSearchScope.allScope(project));
        if (cabalFile == null) return;
        Collection<CabalBuildInformation> buildInformations = PsiTreeUtil.findChildrenOfType(cabalFile,
                CabalBuildInformation
                        .class);
        final HashMap<String, CabalBuildInformation> buildInfosMap = Maps.newHashMap();
        for (CabalBuildInformation buildInformation : buildInformations) {
            CabalBuildDepends buildDepends = buildInformation.getBuildDepends();
            if (buildDepends == null) {
                continue;
            }
            List<CabalDependency> dependencyList = buildDepends.getDependencyList();
            if (dependencyList.size() == 0) {
                continue;
            }
            PsiElement parent = buildInformation.getParent().getParent();
            if (parent instanceof CabalLibrary) {
                buildInfosMap.put("library", buildInformation);
            }
            if (parent instanceof CabalExecutable) {
                CabalExecutable executable = (CabalExecutable) parent;
                buildInfosMap.put(executable.getVarid().getText(), buildInformation);
            }
            if (parent instanceof CabalTestSuite) {
                CabalTestSuite testSuite = (CabalTestSuite) parent;
                buildInfosMap.put(testSuite.getVarid().getText(), buildInformation);
            }
            if (parent instanceof CabalBenchmark) {
                CabalBenchmark benchmark = (CabalBenchmark) parent;
                buildInfosMap.put(benchmark.getVarid().getText(), buildInformation);
            }
        }


        final JList lijstje = new JBList(buildInfosMap.keySet());
        PopupChooserBuilder listPopupBuilder = JBPopupFactory.getInstance().createListPopupBuilder(lijstje);
        JBPopup popup = listPopupBuilder
                .setItemChoosenCallback(new Runnable() {
                    @Override
                    public void run() {
                        WriteCommandAction<Void> writeCommandAction = new WriteCommandAction<Void>(project) {
                            @Override
                            protected void run(@NotNull Result<Void> result) throws Throwable {
                                String selectedBuildInfo = (String) lijstje.getSelectedValue();
                                CabalBuildInformation originalCabalBuildInformation = buildInfosMap.get(selectedBuildInfo);
                                CabalBuildDepends originalBuildDepends = originalCabalBuildInformation.getBuildDepends();
                                List<CabalDependency> dependencyList = originalBuildDepends.getDependencyList();
                                CabalDependency firstDependency = dependencyList.get(0);
                                CabalBuildInformation cabalBuildInformation = CabalElementFactory.createCabalBuildInformation(project, packageName);
                                CabalDependency newDependency = cabalBuildInformation.getBuildDepends().getDependencyList().get(0);
                                PsiElement comma = newDependency.getNextSibling();


                                originalBuildDepends.addAfter(newDependency, firstDependency);
                                originalBuildDepends.addAfter(comma, firstDependency);
                            }
                        };
                        writeCommandAction.execute();
                    }
                })
                .createPopup();
        popup.showInBestPositionFor(editor);

    }


}
