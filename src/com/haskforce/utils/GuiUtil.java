package com.haskforce.utils;

import com.intellij.openapi.externalSystem.util.ExternalSystemUiUtil;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.TextFieldWithHistory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

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
    public static TextFieldWithBrowseButton createExecutableOption(JPanel settings, String tool, Object constraints) {
        // Create UI elements.
        TextFieldWithBrowseButton tf = new TextFieldWithBrowseButton();
        tf.addBrowseFolderListener("Select " + tool + " path", "", null,
                FileChooserDescriptorFactory.createSingleLocalFileDescriptor());

        // Add elements to Panel.
        JPanel subPanel = new JPanel(new GridBagLayout());
        subPanel.add(new JLabel(tool + " executable path:"));
        subPanel.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));
        settings.add(subPanel, constraints);

        return tf;
    }

    public static TextFieldWithBrowseButton createExecutableOption(JPanel settings, String tool) {
        return createExecutableOption(settings, tool, ExternalSystemUiUtil.getFillLineConstraints(0));
    }

    /**
     * Creates a label and text field input and adds them to the configuration
     * window.
     *
     * @param settings Panel to add components to.
     * @param labelText Which text to show to the left of the field.
     * @return The TextFieldWithBrowseButton created.
     */
    public static TextFieldWithHistory createTextfield(JPanel settings, String labelText, Object constraints) {
        // Create UI elements.
        TextFieldWithHistory tf = new TextFieldWithHistory();

        // Add elements to Panel.
        JPanel subPanel = new JPanel(new GridBagLayout());
        subPanel.add(new JLabel(labelText + ':'));
        subPanel.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));
        settings.add(subPanel, constraints);

        return tf;
    }

    public static TextFieldWithHistory createTextfield(JPanel settings, String labelText) {
        return createTextfield(settings, labelText, ExternalSystemUiUtil.getFillLineConstraints(0));
    }

    /**
     * Creates two labels adds them to the configuration window.
     *
     * @param settings Panel to add components to.
     * @param tool Which tool to display version for.
     * @return The label with dynamic content.
     */
    public static JLabel createDisplayVersion(JPanel settings, String tool, Object constraints) {
        JLabel tf = new JLabel("");

        // Add elements to Panel.
        JPanel subPanel = new JPanel(new GridBagLayout());
        subPanel.add(new JLabel(tool + " version:"));
        subPanel.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));
        settings.add(subPanel, constraints);

        return tf;
    }

    public static JLabel createDisplayVersion(JPanel settings, String tool) {
        return createDisplayVersion(settings, tool, ExternalSystemUiUtil.getFillLineConstraints(0));
    }

    /**
     * Create a check box and add to the configuration window.
     *
     * @param settings Panel to add checkbox to.
     * @param text Checkbox text.
     * @return The checkbox.
     */
    public static JCheckBox createCheckBoxOption(JPanel settings, String text, Object constraints) {
        JCheckBox tf = new JCheckBox(text);
        settings.add(tf, constraints);

        return tf;
    }

    public static JCheckBox createCheckBoxOption(JPanel settings, String text) {
        return createCheckBoxOption(settings, text, ExternalSystemUiUtil.getFillLineConstraints(0));
    }

    /**
     * Create a button and add to the configuration window.
     *
     * @param settings Panel to add button to.
     * @param text Button text.
     * @return The button.
     */
    public static JButton createButton(JPanel settings, String text, ActionListener action, Object constraints) {
        JButton button = new JButton(text);
        button.addActionListener(action);
        settings.add(button, constraints);
        return button;
    }

    public static JButton createButton(JPanel settings, String text, ActionListener action) {
        return createButton(settings, text, action, ExternalSystemUiUtil.getFillLineConstraints(0));
    }
}
