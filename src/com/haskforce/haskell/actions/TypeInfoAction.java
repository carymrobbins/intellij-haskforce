package com.haskforce.haskell.actions;

import com.haskforce.tools.ghcmod.TypeInfoUtil;
import com.haskforce.haskell.psi.HaskellFile;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.ui.LightweightHint;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;

/**
 * This action will show the type information on the position of the cursor or the selection.
 * It will create a normal pop up in the neighbourhood of the cursor or selection position
 * containing the types found
 */
public class TypeInfoAction extends AnAction {
    /**
     * Only enable type info when in a Haskell file.
     */
    @Override
    public void update(AnActionEvent e) {
        final PsiFile file = e.getData(CommonDataKeys.PSI_FILE);
        e.getPresentation().setEnabled(file != null && file instanceof HaskellFile);
    }

    public void actionPerformed(@NotNull AnActionEvent e) {
        Project project = e.getProject();
        if (project == null){
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(project);
        //TODO refactor for GHC-Mod abstraction
        String typeInfo = TypeInfoUtil.getTypeInfo(project);
        if (typeInfo == null) {
            // TODO: getTypeInfo should probably be @NotNull
            return;
        }
        JComponent label = HintUtil.createInformationLabel(typeInfo);
        label.setFont(UIUtil.getLabelFont());
        final LightweightHint hint = new LightweightHint(label);
        final HintManagerImpl manager = HintManagerImpl.getInstanceImpl();
        label.addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                manager.hideAllHints();
            }
        });
        Editor editor = fileEditorManager.getSelectedTextEditor();
        // TODO: Refactor this, editor shouldn't be null since it's checked in getTypeInfo
        assert editor != null;
        LogicalPosition position = editor.getCaretModel().getLogicalPosition();
        Point point = HintManagerImpl.getHintPosition(hint, editor, position, HintManager.ABOVE);
        int flags = HintManager.HIDE_BY_ANY_KEY |
                HintManager.HIDE_BY_TEXT_CHANGE |
                HintManager.HIDE_BY_SCROLLING;
        manager.showEditorHint(hint, editor, point, flags, 0, false);
    }
}
