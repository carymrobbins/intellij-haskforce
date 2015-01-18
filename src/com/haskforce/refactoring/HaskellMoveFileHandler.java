package com.haskforce.refactoring;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellFile;
import com.intellij.lang.Language;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.RefactoringActionHandlerFactory;
import com.intellij.refactoring.actions.MoveAction;
import com.intellij.refactoring.move.moveFilesOrDirectories.MoveFileHandler;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.Nullable;

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

    }

    @Nullable
    @Override
    public List<UsageInfo> findUsages(PsiFile psiFile, PsiDirectory psiDirectory, boolean b, boolean b1) {
        return null;
    }

    @Override
    public void retargetUsages(List<UsageInfo> list, Map<PsiElement, PsiElement> map) {

    }

    @Override
    public void updateMovedFile(PsiFile psiFile) throws IncorrectOperationException {

    }
}
