package com.haskforce.features.intentions;


import com.haskforce.cabal.index.CabalFileIndex;
import com.haskforce.cabal.psi.*;
import com.haskforce.cabal.psi.impl.CabalElementFactory;
import com.haskforce.psi.HaskellPsiUtil;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.haskforce.utils.FileUtil;
import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.vfs.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.components.JBList;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.indexing.FileBasedIndex;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Collection;
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
    public void invoke(@NotNull Project project, Editor editor, PsiFile psiFile) throws IncorrectOperationException {
        CabalFile cabalFile = CabalFileIndex.getCabalFileByProjectName(project, GlobalSearchScope.allScope(project));
        if (cabalFile == null) return;
        Collection<CabalBuildInformation> buildInformations = PsiTreeUtil.findChildrenOfType(cabalFile,
                CabalBuildInformation
                .class);
        for (CabalBuildInformation buildInformation : buildInformations) {
            CabalBuildDepends buildDepends = buildInformation.getBuildDepends();
            if (buildDepends == null){
                continue;
            }
            List<CabalDependency> dependencyList = buildDepends.getDependencyList();
            if (dependencyList.size() == 0){
                continue;
            }
            CabalDependency firstDependency = dependencyList.get(0);
            CabalDependency newDependency = CabalElementFactory.createCabalDependency(project, packageName);
            firstDependency.addAfter(HaskellElementFactory.createNewLine(project), firstDependency);
            firstDependency.addAfter(newDependency, firstDependency);
            firstDependency.addAfter(newDependency, CabalElementFactory.createComma(project));
        }
    }
}
