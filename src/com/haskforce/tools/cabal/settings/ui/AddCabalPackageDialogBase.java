package com.haskforce.cabal.settings.ui;

import com.haskforce.ui.ComboModuleProxy;
import com.haskforce.cabal.settings.CabalComponentType;
import com.haskforce.ui.SComboBox;

import javax.swing.*;
import java.awt.event.*;

/**
 * Base dialog class for adding a Cabal package.  Remains in Java to easily bind to
 * IntelliJ's form designer.  Additional functionality should be added to the
 * AddCabalPackageDialog Scala class.
 */
public abstract class AddCabalPackageDialogBase extends JDialog {

    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;

    protected SComboBox<ComboModuleProxy> packageName;
    protected JTextField cabalVersion;
    protected JTextField author;
    protected SComboBox<String> license;
    protected JTextField email;
    protected JTextField homepage;
    protected JTextField synopsis;
    protected SComboBox<String> category;
    protected SComboBox<CabalComponentType> buildType;
    protected SComboBox<String> language;
    protected JTextField sourceDir;
    protected JTextField version;
    protected JTextField rootDir;
    protected JCheckBox generateComments;

    public AddCabalPackageDialogBase() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        buttonCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        });

        // call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        // call onCancel() on ESCAPE
        contentPane.registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    protected void onOK() {
        dispose();
    }

    private void onCancel() {
        dispose();
    }
}
