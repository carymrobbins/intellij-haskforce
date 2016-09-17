package com.haskforce.language.formatting;

import com.haskforce.haskell.HaskellLanguage;
import com.intellij.application.options.IndentOptionsEditor;
import com.intellij.lang.Language;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.LanguageCodeStyleSettingsProvider;
import org.jetbrains.annotations.NotNull;

/**
 * Provides the indentation options editor for Haskell.
 */
public class HaskellLanguageCodeStyleSettingsProvider extends LanguageCodeStyleSettingsProvider {
    @NotNull
    @Override
    public Language getLanguage() {
        return HaskellLanguage.INSTANCE;
    }

    @Override
    public String getCodeSample(@NotNull SettingsType settingsType) {
        return "";
    }

    @Override
    public IndentOptionsEditor getIndentOptionsEditor() {
        return new HaskellIndentOptionsEditor();
    }

    @Override
    public CommonCodeStyleSettings getDefaultCommonSettings() {
        CommonCodeStyleSettings commonCodeStyleSettings = new CommonCodeStyleSettings(HaskellLanguage.INSTANCE);
        CommonCodeStyleSettings.IndentOptions indentOptions = commonCodeStyleSettings.initIndentOptions();
        indentOptions.INDENT_SIZE = 4;
        indentOptions.TAB_SIZE = 8;
        return commonCodeStyleSettings;
    }
}
