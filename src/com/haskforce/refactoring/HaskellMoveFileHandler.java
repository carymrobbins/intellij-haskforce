package com.haskforce.refactoring;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.haskforce.psi.*;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.haskforce.utils.FileUtil;
import com.intellij.find.findUsages.FindUsagesManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.move.moveFilesOrDirectories.MoveFileHandler;
import com.intellij.refactoring.util.MoveRenameUsageInfo;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.Query;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public class HaskellMoveFileHandler extends MoveFileHandler {


    private PsiDirectory psiDirectory;

    @Override
    public boolean canProcessElement(PsiFile psiFile) {
        return psiFile instanceof HaskellFile;
    }

    @Override
    /**
     * Moved file creates a map with From -> To elements. This map will be used in the retargetUsages function.
     * For every constructor that needs to be renamed we need to add that psielement to From-To wise in the map.
     * The find usages function will then find all usages of these constructor, and the reanamin will be done in
     * the retarget usages function. As far as I understood. seems doable, but what when we do not simply
     * rename a constructor but have to remove it (three deep -> two deep)
     */
    public void prepareMovedFile(PsiFile psiFile, PsiDirectory psiDirectory, Map<PsiElement, PsiElement> map) {
        this.psiDirectory = psiDirectory;
        Project project = psiFile.getProject();
        // 2 of 4
        HaskellModuledecl haskellModuledecl = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
        List<String> subDirs = FileUtil.getPathFromSourceRoot(project, psiDirectory.getVirtualFile());
        List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
        HaskellConid moduleName = Iterables.getLast(conidList);

        PsiReference[] moduleNameReferences = moduleName.getReferences();

        for (PsiReference moduleNameReference : moduleNameReferences) {
            HaskellConid reference = (HaskellConid) (moduleNameReference.getElement());
            PsiElement oldQconid = PsiTreeUtil.getParentOfType(reference, HaskellQconid.class);

            HaskellQconid newQconId = HaskellElementFactory.createQconidFromText(project, subDirs, moduleName.getText());
            /**
             * and indeed, we replace the module name with it's parent.
             * We could avoid this hack most likely if we extended the HaskellReference
             * to link the module declarations together. So instead of resolving the sub elements
             * of A.B.ModuleName (A, B and ModuleName) we resolve the combo.
             */
            map.put(oldQconid,newQconId);

        }
    }

    @Nullable
    @Override
    public List<UsageInfo> findUsages(PsiFile psiFile, PsiDirectory newParent, boolean searchInComments,
                                      boolean searchInOtherFiles) {
        HaskellModuledecl haskellModuledecl = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
        List<UsageInfo> usageInfos = Lists.newArrayList();
        HaskellQconid haskellQconid = haskellModuledecl.getQconid();
        Collection<PsiReference> psiReferences = ReferencesSearch.search(haskellQconid, GlobalSearchScope.allScope(psiFile.getProject())).findAll();
        for (PsiReference psiReference : psiReferences) {
            UsageInfo usageInfo = new MoveRenameUsageInfo(psiReference, haskellQconid);
            usageInfos.add(usageInfo);
        }
        return usageInfos;
    }

    @Override
    public void retargetUsages(List<UsageInfo> list, Map<PsiElement, PsiElement> oldToNewMap) {
        // 4 of 4
        /**
         This renames the usages.
         **/
        for (UsageInfo usageInfo : list) {
            if (usageInfo instanceof MoveRenameUsageInfo){
                MoveRenameUsageInfo moveRenameUsageInfo = (MoveRenameUsageInfo) usageInfo;
                PsiElement oldElement = moveRenameUsageInfo.getReferencedElement();
                PsiElement newElement = oldToNewMap.get(oldElement);
                PsiReference reference = moveRenameUsageInfo.getReference();
                reference.bindToElement(newElement);
            }
        }

    }

    @Override
    public void updateMovedFile(PsiFile psiFile) throws IncorrectOperationException {
        // 3 of 4
        /**
         This is for 'post move' actions. The file's current directory should already be updated and so. Nothing more to
         do here for the haskell side of things.
         */
        if(psiFile instanceof HaskellFile){
            /**
             * TODO Very similar to what happens in 'prepareMovedFile'. Check whether we can extract some common 
             * funcionality as soon as we're sure it all works and have enough tests.
             */
            HaskellModuledecl haskellModuledecl = PsiTreeUtil.getChildOfType(psiFile, HaskellModuledecl.class);
            HaskellConid moduleName = Iterables.getLast(haskellModuledecl.getQconid().getConidList());
            Project project = psiFile.getProject();
            List<String> subDirs = FileUtil.getPathFromSourceRoot(project, psiDirectory.getVirtualFile());
            HaskellQconid newQconId = HaskellElementFactory.createQconidFromText(project, subDirs, moduleName.getText());
            HaskellQconid oldQconid = haskellModuledecl.getQconid();
            oldQconid.replace(newQconId);
        }
    }
}
