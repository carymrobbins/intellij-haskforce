# Modules

To create a custom module, you'll want to create implementations of the following -

* `com.intellij.openapi.module.ModuleType` - This is simply a tag which helps to identify
  what type of Module we have.
  
* `com.intellij.ide.util.projectWizard.ModuleBuilder` - This builds the module,
  setting up its content entries (source roots, excludes, libraries, etc.)
  
* `com.intellij.openapi.roots.ui.configuration.DefaultModuleEditorsProvider` -
  This provides the appropriate editors that are available to a given Module.

Implementations for these exist in this project, so use them as a reference.
