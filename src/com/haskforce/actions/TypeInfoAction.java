package com.haskforce.actions;

import com.haskforce.highlighting.annotation.external.TypeInfoProvider;
import com.haskforce.highlighting.annotation.external.TypeInfoProviderFactory$;
import com.haskforce.highlighting.annotation.external.impl.DefaultTypeInfoProvider;
import com.haskforce.psi.HaskellFile;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.ui.LightweightHint;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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

    private static Logger LOG = Logger.getInstance(TypeInfoAction.class);

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
            LOG.warn("Cannot obtain type info: project is null");
            return;
        }
        String typeInfo = getTypeInfo(project);
        if (typeInfo == null) return;
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
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        assert editor != null;
        manager.showEditorHint(hint, editor, getHintPoint(editor, hint), HINT_FLAGS, 0, false);
    }

    @SuppressWarnings("FieldCanBeLocal")
    private static int HINT_FLAGS =
      HintManager.HIDE_BY_ANY_KEY
        | HintManager.HIDE_BY_TEXT_CHANGE
        | HintManager.HIDE_BY_SCROLLING;

    private static Point getHintPoint(Editor editor, LightweightHint hint) {
        // Shouldn't be null since it's leveraged by the TypeInfoProviders.
        LogicalPosition position = editor.getCaretModel().getLogicalPosition();
        return HintManagerImpl.getHintPosition(hint, editor, position, HintManager.ABOVE);
    }

    @Nullable
    public static String getTypeInfo(Project project) {
        return TypeInfoProviderFactory$.MODULE$.mkInput(project).fold(
          failure -> {
              LOG.warn("Failed to obtain input for type info: " + failure);
              return null;
          },
          input -> DefaultTypeInfoProvider.get(input).getTypeInfo().fold(
            failure -> {
                LOG.warn("Failed to obtain type info: " + failure.message());
                return null;
            },
            TypeInfoProvider.Success::typeInfo
          )
        );
    }
}
