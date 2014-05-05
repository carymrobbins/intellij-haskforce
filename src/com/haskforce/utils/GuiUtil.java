package com.haskforce.utils;

import com.intellij.openapi.externalSystem.util.ExternalSystemUiUtil;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;

import javax.swing.*;

/**
 * Various helpers for creating GUI elements.
 */
public class GuiUtil {
    /**
     * Creates a label and path selector and adds them to the configuration
     * window.
     *
     * @param settings Panel to add components to.
     * @param tool Which tool to configure.
     * @return The TextFieldWithBrowseButton created.
     */
    public static TextFieldWithBrowseButton createExecutableOption(JPanel settings, String tool) {
        // Create UI elements.
        TextFieldWithBrowseButton tf = new TextFieldWithBrowseButton();
        tf.addBrowseFolderListener("Select " + tool + " path", "", null,
                FileChooserDescriptorFactory.createSingleLocalFileDescriptor());

        // Add elements to Panel.
        settings.add(new JLabel(tool + " executable path:"));
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }

    /**
     * Creates two labels adds them to the configuration window.
     *
     * @param settings Panel to add components to.
     * @param tool Which tool to display version for.
     * @return The label with dynamic content.
     */
    public static JLabel createDisplayVersion(JPanel settings, String tool) {
        JLabel tf = new JLabel("");

        // Add elements to Panel.
        settings.add(new JLabel(tool + " version:"));
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }

    /**
     * Create a check box and add to the configuration window.
     *
     * @param settings Panel to add checkbox to.
     * @param text Checkbox text.
     * @return The checkbox.
     */
    public static JCheckBox createCheckBoxOption(JPanel settings, String text) {
        JCheckBox tf = new JCheckBox(text);
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }
}
