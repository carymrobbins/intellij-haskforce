package com.haskforce.haskell.language.formatting;

import com.haskforce.haskell.HaskellLanguage;
import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.Language;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsProvider;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Provides the settings panel for Haskell in the Code Style section.
 */
public class HaskellCodeStyleSettingsProvider extends CodeStyleSettingsProvider {

    @NotNull
    @Override
    public Configurable createSettingsPage(CodeStyleSettings settings, CodeStyleSettings originalSettings) {
        return new CodeStyleAbstractConfigurable(settings, originalSettings, "Haskell") {
            @Override
            protected CodeStyleAbstractPanel createPanel(CodeStyleSettings settings) {
                return new HaskellTabbedCodeStylePanel(getCurrentSettings(), settings);
            }

            @Nullable
            @Override
            public String getHelpTopic() {
                return null;
            }
        };
    }

    @Override
    public Language getLanguage() {
        return HaskellLanguage.INSTANCE;
    }

    @Override
    public String getConfigurableDisplayName() {
        return "Haskell";
    }

}
