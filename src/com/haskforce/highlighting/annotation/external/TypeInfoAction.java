package com.haskforce.highlighting.annotation.external;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.ui.awt.RelativePoint;

public class TypeInfoAction extends AnAction {
    public void actionPerformed(AnActionEvent e) {
        Project project = e.getProject();
        if (project == null){
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(project);
        String typeInfo = TypeInfoUtil.getTypeInfo(project);
        JBPopupFactory jbPopupFactory = JBPopupFactory.getInstance();
        Editor selectedTextEditor = fileEditorManager.getSelectedTextEditor();
        if (selectedTextEditor == null){
            return;
        }
        RelativePoint relativePoint = jbPopupFactory.guessBestPopupLocation(
                selectedTextEditor);
        JBPopup message = jbPopupFactory.createMessage(typeInfo);
        message.show(relativePoint);
    }
}
