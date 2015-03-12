package com.haskforce.refactoring;

import com.haskforce.psi.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.testFramework.PsiTestUtil;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.List;
import java.util.Map;

public class HaskellRenamePsiElementProcessor extends RenamePsiElementProcessor {

    @Nullable
    @Override
    public PsiElement substituteElementToRename(PsiElement element, Editor editor) {
        /**
         * If element to rename is a module or an import -> equivalent to a rename of the file
         */
        HaskellModuledecl haskellModuledecl = PsiTreeUtil.getParentOfType(element, HaskellModuledecl.class);
        if (haskellModuledecl != null){
            List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
            if(conidList.size() > 0){
                HaskellConid haskellConid = conidList.get(conidList.size()-1);
                if(element.equals(haskellConid)){
                    return element.getContainingFile();
                }
            }
        }

        HaskellImpdecl haskellImpdecl = PsiTreeUtil.getParentOfType(element, HaskellImpdecl.class);
        if(haskellImpdecl != null){
            /**
             * TODO
             * This might also be something to move to the HaskellImpdecl
             */
            List<HaskellQconid> qconidList = haskellImpdecl.getQconidList();
            if(qconidList.size() >0){
                HaskellQconid haskellQconid = qconidList.get(0);
                List<HaskellConid> conidList = haskellQconid.getConidList();
                if(conidList.size() > 0){
                    HaskellConid haskellConid = conidList.get(conidList.size() - 1);
                    if(element.equals(haskellConid)){
                        return element.getContainingFile();
                    }
                }
            }
        }
        return super.substituteElementToRename(element, editor);
    }

    @Override
    public void prepareRenaming(PsiElement element, String newName, Map<PsiElement, String> allRenames) {
        if (element instanceof HaskellFile){
            HaskellModuledecl haskellModuledecl = PsiTreeUtil.getChildOfType(element, HaskellModuledecl.class);
            if (haskellModuledecl != null){
                /**
                 * TODO duplication (also in substituteElementToRename for example), move this to the
                 * HaskellModuledecl element itself !?
                 */
                List<HaskellConid> conidList = haskellModuledecl.getQconid().getConidList();
                if (conidList.size() >0){
                    HaskellConid haskellConid = conidList.get(conidList.size()-1);
                    String newModuleName = createCorrectModuleName(newName);
                    String newFileName = createCorrectFileName(newName);
                    allRenames.put(haskellConid, newModuleName);
                    allRenames.put(element,newFileName);
                }
            }
        }
        super.prepareRenaming(element, newName, allRenames);
    }

    private String createCorrectModuleName(String newName) {
        if (StringUtils.endsWith(newName,".hs")){
            return StringUtils.removeEnd(newName,".hs");
        } else {
            return newName;
        }
    }

    private String createCorrectFileName(String newName) {
        if (StringUtils.endsWith(newName,".hs")){
            return newName;
        } else {
            return newName + ".hs";
        }
    }

    @Override
    public boolean canProcessElement(PsiElement psiElement) {
        return (psiElement instanceof HaskellNamedElement || psiElement instanceof HaskellFile);
    }
}
