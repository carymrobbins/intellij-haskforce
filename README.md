# HaskForce [![Build Status](https://travis-ci.org/carymrobbins/intellij-haskforce.svg?branch=master)](https://travis-ci.org/carymrobbins/intellij-haskforce)

The IntelliJ plugin for Haskell.
--------

Want to get started right away?  Check out the [Quick Start Guide](https://github.com/carymrobbins/intellij-haskforce/wiki/Quick-Start-Guide)!

Building with IntelliJ
--------
**Note:** We currently use JDK 6 for building the plugin.

1. Copy the `build.skeleton.properties` to `build.properties` and update the paths to the JDK and IntelliJ installation.
1. (Optional) Check out the Community Edition source files.
  * `$ git clone https://github.com/JetBrains/intellij-community.git idea`
  * Check the version of your IntelliJ installation by going to About IntelliJ IDEA and checking the
    build number.
  * Check out the appropriate tag for your build number.  For instance, you have Build #IC-135.909, then do
    `$ git checkout idea/135.909`
  * Be sure to `git checkout` the new tag each time you upgrade IntelliJ.
1. Install and enable additional plugins.
  * PsiViewer 3.28.73
  * JFlex Support 1.5.1 (optional, for generating lexers)
  * Grammar Kit 1.2.0.2 (optional, for generating parsers - [download it from here](https://github.com/JetBrains/Grammar-Kit/releases/tag/1.2.0.2))
  * Ant Support
1. Configure SDK and source files.
  * Go to **File > Project Structure**.  Add SDKs for JDK and IDEA Plugins.
  * For **Project SDK** choose **New > Intellij Platform Plugin SDK**.
  * Update the **Project compiler output**, e.g. `path/to/intellij-haskforce/out`
  * If you cloned the IntelliJ sources, go to **SDKs**, choose your IntelliJ Plugin SDK, and update the **Sourcepath**
    to point to the cloned IntelliJ sources.
1. Generate lexers by hovering over the opening the **Ant Build** tool and choosing **generate.sources**.
  * Alternatively, if you have the `ant` command line tool you can run `ant generate.sources` from the project root.
1. Choose **Build > Prepare Plugin Module 'intellij-haskforce' for Deployment**.

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
