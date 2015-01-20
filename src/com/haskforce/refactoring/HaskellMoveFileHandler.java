package com.haskforce.refactoring;

import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellModuledecl;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.move.moveClassesOrPackages.MoveClassesOrPackagesUtil;
import com.intellij.refactoring.move.moveFilesOrDirectories.MoveFileHandler;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ProcessingContext;
import org.apache.velocity.util.StringUtils;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

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
    public List<UsageInfo> findUsages(PsiFile psiFile, PsiDirectory newParent, boolean searchInComments,
                                      boolean searchInOtherFiles) {
        /**
         * KIVVVSS : hierarchy of one directory deep for now. Only move from one directory deep to one directory deep.
         * Feeling my way around here.
         */

        PsiReferenceProvider referenceProvider = ServiceManager.getService(PsiReferenceProvider.class);



        HaskellModuledecl haskellModuledecl = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
        List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
        HaskellConid firstCon = conidList.get(0);
        PsiReference[] referencesByElement = referenceProvider.getReferencesByElement(firstCon, new ProcessingContext());

        return null;
        /*String currentDirectory = psiFile.getContainingDirectory().getName();
        String futureDirectory = newParent.getName();
        List<UsageInfo> result = new ArrayList<UsageInfo>();
        List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
        for (HaskellConid haskellConid : conidList) {
            if (haskellConid.getText().equals(futureDirectory)){
                UsageInfo[] usages = MoveClassesOrPackagesUtil.findUsages(haskellConid, true, true, "koekoek");
                UsageInfo[] usages = MoveClassesOrPackagesUtil.findUsages(haskellConid, true, true, "koekoek");
                Collections.addAll(result, usages);
            }
        }*/
    }

    @Override
    public void retargetUsages(List<UsageInfo> list, Map<PsiElement, PsiElement> oldToNewMap) {
        System.out.println("retargetUsages");
    }

    @Override
    public void updateMovedFile(PsiFile psiFile) throws IncorrectOperationException {
        String[] constructorNames = StringUtils.split(psiFile.getContainingDirectory().getName(), ".");
        HaskellFile haskellFile = (HaskellFile) psiFile;
        HaskellModuledecl haskellModuledecl = PsiTreeUtil.findChildOfType(haskellFile, HaskellModuledecl.class);

        List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
        /**
         * KIVVVSS : hierarchy of one directory deep for now. Only move from one directory deep to one directory deep.
         * Feeling my way around here.
         */
        conidList.get(0).setName(constructorNames[0]);
    }
}
