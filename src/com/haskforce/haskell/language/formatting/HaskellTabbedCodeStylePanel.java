package com.haskforce.language.formatting;

import com.haskforce.haskell.HaskellLanguage;
import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.psi.codeStyle.CodeStyleSettings;

/**
 * A bare-bones Code Style panel for Haskell.
 * Overrides {@link super#initTabs(com.intellij.psi.codeStyle.CodeStyleSettings)} (which adds several tabs), so that
 * only an indent options tab is added.
 */
public class HaskellTabbedCodeStylePanel extends TabbedLanguageCodeStylePanel {

    public HaskellTabbedCodeStylePanel(CodeStyleSettings currentSettings, CodeStyleSettings settings) {
        super(HaskellLanguage.INSTANCE, currentSettings, settings);
    }

    @Override
    protected void initTabs(CodeStyleSettings settings) {
        addIndentOptionsTab(settings);
    }
}
