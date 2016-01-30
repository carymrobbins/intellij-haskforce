# HaskForce [![Build Status](https://travis-ci.org/carymrobbins/intellij-haskforce.svg?branch=master)](https://travis-ci.org/carymrobbins/intellij-haskforce)

## The IntelliJ plugin for Haskell.

Want to get started right away?  Check out the [Quick Start Guide](https://github.com/carymrobbins/intellij-haskforce/wiki/Quick-Start-Guide)!

## Getting involved

Want to contribute code?  See the [CONTRIBUTING](./CONTRIBUTING.md) doc for more info.

Follow [@HaskForce](https://twitter.com/haskforce) on Twitter to stay up to date
on new releases and work in progress.

Do you IRC?  Join **#haskforce** on freenode!

## Prerequisites for building

You will need Scala 2.11.7 on your system.  You can use the IvyIDEA plugin to resolve all dependencies,
including Scala (see **Building with IntelliJ** below).

You can configure Scala manually in IntelliJ; however, this is **not recommended**.
Ignoring forewarning, you can download it directly via IntelliJ by going to -
  * **File > Project Structure > Global Libraries > + > Scala SDK > Download > 2.11.7**
Be sure that the SDK is named `scala-sdk-2.11.7` to properly match the module configurations.

## Building with IntelliJ

**Note:** We currently target JDK 6 for building the plugin.

1. Copy the `build.skeleton.properties` to `build.properties` and update the paths to the JDK and IntelliJ installation.
1. Install and enable additional plugins.
  * **Scala**
  * **Ant Support**
  * **IvyIDEA** (resolve dependencies via **Tools > IvyIDEA > Resolve for all modules**)
  * **UI Designer** (without this everything compiles but viewing forms throws NPEs, e.g. when creating a new project.)
  * **PsiViewer 3.28.73** (optional, for viewing parse trees)
  * **JFlex Support 1.5.1** (optional, for generating lexers)
  * **Grammar Kit 1.2.1** (optional, for generating parsers)
1. Configure SDK and source files.
  * Go to **File > Project Structure**.  Add SDKs for JDK and IDEA Plugins.
  * For **Project SDK** choose **New > Intellij Platform Plugin SDK**.
  * Update the **Project compiler output**, e.g. `path/to/intellij-haskforce/out`
1. Generate lexers by hovering over the opening the **Ant Build** tool and choosing **generate.sources**.
  * Alternatively, if you have the `ant` command line tool you can run `ant generate.sources` from the project root.
1. Choose **Build > Prepare Plugin Module 'intellij-haskforce' for Deployment**.

## Adding IntelliJ Sources (Optional)

1. Check out the Community Edition source files.
  * `$ git clone https://github.com/JetBrains/intellij-community.git idea`
  * Check the build version of your IntelliJ installation.  There are two ways to do this.
    1. Look for a `build.txt` file in your IntelliJ installation directory.
       It's contents should be something like `IU-141.178.9`
    1. Alternatively, you can go to **About** in the menu and look at the build number.  This might not have the minor
       number, e.g. instead of `IU-141.178.9` it might just say `IU-141.178`
  * Check out the appropriate tag for your build number.
    * `$ git fetch --tags && git checkout idea/141.178.9`
  * Be sure to `git checkout` the new tag each time you upgrade IntelliJ.
2. Under **File > Project Structure > SDKs** find your **IntelliJ Platform Plugin SDK**.
3. Under the **Sourcepath** tab, add the directory where you cloned the IntelliJ sources.

## Running the plugin

1. From the menu go to **Run > Edit Configurations**
1. Click on the `+` sign and choose **Plugin**, name the configuration something evocative like **Haskforce**, click **OK**, then run your new configuration.
1. Intellij will open a copy of itself, with default settings, and the plugin installed.

## Testing the plugin

To run the tests, you'll need to create a run configuration:

1. Go to **Run > Edit Configurations**
1. Click on the `+` sign and choose **JUnit**
1. In the Class field enter **HaskellTestCase**, which should auto-complete for you.
1. Click **OK** and run your new test configuration.

To add more tests:

* Edit Haskell\*Test.java files to add more tests of the same kind that already exists.
* Edit HaskellTestCase.java if you need to add tests of a different kind.


