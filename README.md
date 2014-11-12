# HaskForce [![Build Status](https://travis-ci.org/carymrobbins/intellij-haskforce.svg?branch=master)](https://travis-ci.org/carymrobbins/intellij-haskforce)

The IntelliJ plugin for Haskell.
--------

Want to get started right away?  Check out the [Quick Start Guide](https://github.com/carymrobbins/intellij-haskforce/wiki/Quick-Start-Guide)!

Building the plugin
--------

1. Clone HaskForce.

1. If you haven't already, download and install IntelliJ IDEA and Java JDK.

1. (Optional) Check out the Community Edition source files.
  * `$ git clone https://github.com/JetBrains/intellij-community.git idea`
  * Check the version of your IntelliJ installation by going to About IntelliJ IDEA and checking the
    build number.
  * Check out the appropriate tag for your build number.  For instance, you have Build #IC-135.909, then do
    `$ git checkout idea/135.909`
  * Be sure to `git checkout` the new tag each time you upgrade IntelliJ.
1. Install and enable additional plugins.
  * JFlex Support
  * Plugin DevKit (already installed)
  * PsiViewer
1. Configure SDK and source files.
  * Create a new **IntelliJ Platform Plugin** project from existing sources (pointed to your cloned HaskForce directory).
  * Go to File > Project Structure.  Add SDKs for JDK and IDEA Plugins.  For the IDEA Plugins, add sources
    from the cloned idea to the Sourcepath, if applicable.
1. Set the project SDK to the IDEA SDK.
1. Generate lexers using **Build > haskforce > generate.sources**.  Alternatively, if you have the `ant` command line
   tool you can run `ant generate.sources` from the project root.
1. Choose **Build > Make Project**. If you want to see that something actually happened, you can open the log with **View > Tool Windows > Event Log**.


Running the plugin
--------
1. From the menu go to **Run > Edit Configurations**
1. Click on the `+` sign and choose **Plugin**, name the configuration something evocative like **Haskforce**, click **OK**, then run your new configuration.
1. Intellij will open a copy of itself, with default settings, and the plugin installed.

Testing the plugin
--------

To run the tests, you'll need to create a run configuration:

1. Go to **Run > Edit Configurations**
1. Click on the `+` sign and choose **JUnit**
1. In the Class field enter **HaskellTestCase**, which should auto-complete for you.
1. Click **OK** and run your new test configuration.

To add more tests:

* Edit Haskell\*Test.java files to add more tests of the same kind that already exists.
* Edit HaskellTestCase.java if you need to add tests of a different
  kind.
