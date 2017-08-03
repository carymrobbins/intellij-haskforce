# Modules


To create a custom module, you'll want to create implementations of the following -
* ModuleType - This is simply a tag which helps to identify what type of Module we have.
* ModuleBuilder - This builds the module, setting up its content entries (source roots,
    excludes, libraries, etc.)
* DefaultModuleEditorsProvider - This provides the appropriate editors that are
    available to a given Module.

Implementations for these exist in this project, so use them as a reference.
