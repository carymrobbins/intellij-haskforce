package com.haskforce.language.formatting;

import com.haskforce.HaskellLanguage;
import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.psi.codeStyle.CodeStyleSettings;

public class HaskellTabbedCodeStylePanel extends TabbedLanguageCodeStylePanel {

    public HaskellTabbedCodeStylePanel(CodeStyleSettings currentSettings, CodeStyleSettings settings) {
        super(HaskellLanguage.INSTANCE, currentSettings, settings);
    }

    @Override
    protected void initTabs(CodeStyleSettings settings) {
        addIndentOptionsTab(settings);
    }
}
