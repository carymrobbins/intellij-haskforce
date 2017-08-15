# Run Configurations

To create custom run configurations, you'll want to create implementations
of the following -

* `com.intellij.execution.configurations.ConfigurationTypeBase` - This class will exist
  as a singleton with basic metadata describing the run config. You can obtain
  the singleton instance with `Extensions.findExtension(CONFIGURATION_TYPE_EP, ...)`.
  It will need to be passed to your implementation of `RunConfigurationProducer`.
  * Requires extension point `configurationType` in plugin.xml

* `com.intellij.execution.actions.RunConfigurationProducer` - This class enables
  the user to create a run configuration from a right-click context menu, either
  from the source code or from the project file tree.
  * Requires extension point `runConfigurationProducer` in plugin.xml

* `com.intellij.execution.configuration.RunConfiguration` - Creates the state
  (usually a `CommandLineState`), run config editor, and reads and writes the
  run config options to the file system.

* `com.intellij.openapi.options.SettingsEditor` - Creates a UI form and syncs changes
  between the form and your `RunConfiguration`.

* `com.intellij.execution.configurations.CommandLineState` - While you can alternatively
  create an implementation of this class' superclass, `RunProfileState`, you most commonly
  will just need a `CommandLineState`. All this needs to do is create a command line process
  and return it as a `ProcessHandler`.

Implementations for these exist in this project, so use them as a reference.
