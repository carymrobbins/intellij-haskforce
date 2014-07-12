package com.haskforce.settings;

import com.haskforce.HaskellModuleType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleConfigurationEditor;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.roots.ui.configuration.ClasspathEditor;
import com.intellij.openapi.roots.ui.configuration.DefaultModuleEditorsProvider;
import com.intellij.openapi.roots.ui.configuration.JavaContentEntriesEditor;
import com.intellij.openapi.roots.ui.configuration.ModuleConfigurationState;

/**
 * Project Settings->Module panel.
 */
public class HaskellModuleConfigurationEditor extends DefaultModuleEditorsProvider {
    public ModuleConfigurationEditor[] createEditors(ModuleConfigurationState state) {
        Module module = state.getRootModel().getModule();
        if (!(ModuleType.get(module) instanceof HaskellModuleType)) {
            return ModuleConfigurationEditor.EMPTY;
        }
        return new ModuleConfigurationEditor[]{
                new JavaContentEntriesEditor(module.getName(), state),
                // new CabalFilesEditor(state),
                new ClasspathEditor(state),
        };
    }
}
