package com.haskforce.language.formatting;

import com.intellij.application.options.IndentOptionsEditor;

/**
 * Indent options editor for Haskell.
 * Overrides {@link super#setEnabled(boolean)} to disable the tab size field. The
 * <a href="https://www.haskell.org/onlinereport/haskell2010/haskellch10.html">Haskell standard</a> specifies an
 * 8-character width for tab stops, so it should not be modifiable.
 */
public class HaskellIndentOptionsEditor extends IndentOptionsEditor {

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        myTabSizeField.setEnabled(false);
        myTabSizeLabel.setEnabled(false);
    }
}
