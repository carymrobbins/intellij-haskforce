package com.haskforce.eta.project.module

import com.intellij.openapi.module.{ModuleConfigurationEditor, ModuleType}
import com.intellij.openapi.roots.ui.configuration.{ClasspathEditor, DefaultModuleEditorsProvider, JavaContentEntriesEditor, ModuleConfigurationState}

/** Provides the editors available in Project Structure > Modules. */
class EtlasModuleConfigurationEditor extends DefaultModuleEditorsProvider {

  override def createEditors(state: ModuleConfigurationState): Array[ModuleConfigurationEditor] = {
    val module = state.getRootModel.getModule
    ModuleType.get(module) match {
      case _: EtlasModuleType =>
        Array(
          new JavaContentEntriesEditor(module.getName, state),
          new ClasspathEditor(state)
        )

      case _ =>
        ModuleConfigurationEditor.EMPTY
    }
  }
}
