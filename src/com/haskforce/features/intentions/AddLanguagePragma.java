//package com.haskforce.features.intentions;
//
//import com.haskforce.utils.FileUtil;
//import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
//import com.intellij.openapi.editor.Editor;
//import com.intellij.openapi.project.Project;
//import com.intellij.psi.PsiFile;
//import com.intellij.util.Function;
//import com.intellij.util.IncorrectOperationException;
//import org.jetbrains.annotations.NotNull;
//
//public class AddLanguagePragma extends BaseIntentionAction {
//    public final String languageName;
//
//    public AddLanguagePragma(String languageName) {
//        this.languageName = languageName;
//    }
//
//    @NotNull
//    @Override
//    public String getFamilyName() {
//        return "Add language pragma";
//    }
//
//    @NotNull
//    @Override
//    public String getText() {
//        return "Add " + languageName + " language pragma";
//    }
//
//    @Override
//    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
//        // TODO: Add a setting for this.
//        return true;
//    }
//
//    @Override
//    public void invoke(@NotNull final Project project, Editor editor, final PsiFile file) throws IncorrectOperationException {
//        FileUtil.updateFileText(project, file, new Function<String, String>() {
//            public String fun(String text) {
//                return "{-# LANGUAGE " + languageName + " #-}\n" + text;
//            }
//        });
//    }
//}
