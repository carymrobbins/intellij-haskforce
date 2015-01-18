package com.haskforce.refactoring;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellModuledecl;
import com.intellij.lang.Language;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.RefactoringActionHandlerFactory;
import com.intellij.refactoring.actions.MoveAction;
import com.intellij.refactoring.move.moveClassesOrPackages.MoveClassesOrPackagesUtil;
import com.intellij.refactoring.move.moveFilesOrDirectories.MoveFileHandler;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Created by kasper on 18/01/15.
 */
public class HaskellMoveFileHandler extends MoveFileHandler {


    @Override
    public boolean canProcessElement(PsiFile psiFile) {
        return psiFile instanceof HaskellFile;
    }

    @Override
    public void prepareMovedFile(PsiFile psiFile, PsiDirectory psiDirectory, Map<PsiElement, PsiElement> map) {
        System.out.println("prepareMovedFile");
    }

    @Nullable
    @Override
    public List<UsageInfo> findUsages(PsiFile psiFile, PsiDirectory psiDirectory, boolean b, boolean b1) {
        HaskellModuledecl childOfType = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
        String currentDirectory = psiFile.getContainingDirectory().getName();
        String futureDirectory = psiDirectory.getName();
        List<UsageInfo> result = new ArrayList<UsageInfo>();
        List<HaskellConid> conidList = childOfType.getQconid().getConidList();
        for (HaskellConid haskellConid : conidList) {
            if (haskellConid.getText().equals(futureDirectory)){
                UsageInfo[] usages = MoveClassesOrPackagesUtil.findUsages(haskellConid, true, true, "koekoek");
                Collections.addAll(result, usages);
            }

        }
        return result.isEmpty()? null : result;
    }

    @Override
    public void retargetUsages(List<UsageInfo> list, Map<PsiElement, PsiElement> map) {
        System.out.println("retargetUsages");
    }

    @Override
    public void updateMovedFile(PsiFile psiFile) throws IncorrectOperationException {
        System.out.println("updateMovedFile");
    }
}
