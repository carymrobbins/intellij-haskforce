HaskForce - The IntelliJ plugin for Haskell.
=========

***This plugin is in its early stages and is not ready for use.***

Building the plugin
--------

1. Clone HaskForce.

1. Download and install IntelliJ IDEA.

1. Check out the Community Edition source files.

    ````$ git clone git@github.com:JetBrains/intellij-community.git idea````

1. Configure SDK and source files.
  * Create a new project from existing sources (pointed to your cloned HaskForce directory).
  * Go to File > Project Structure.  Add SDKs for JDK and IDEA Plugins.  For the IDEA Plugins, add sources
    from cloned IntelliJ to the Sourcepath.
1. Install and enable additional plugins.
  * Grammar-Kit
  * JFlex Support
  * Plugin DevKit (already installed)
  * PsiViewer
1. Configure JFlex settings.
  * Go to Preferences.  Below the IDE Settings section locate JFlex.  Set the path and skeleton to the
    idea/tools/lexer files.
1. Set the project SDK to the IDEA SDK.

Testing the plugin
--------

To run the tests, you'll need to create a run configuration:

1. Go to **Run > Edit Configurations**
1. Click on the `+` sign and choose **JUnit**
1. In the Class field enter **HaskellTestCase**, which should auto-complete for you.
1. Click **OK** and run your new test configuration.

To add more tests:

* Edit Haskell*Test.java files to add more tests of the same kind that already exists.
* Edit HaskellTestCase.java if you need to add tests of a different
  kind.
