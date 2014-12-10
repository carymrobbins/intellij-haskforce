package com.haskforce.ui;

import com.intellij.ui.TextAccessor;

import javax.swing.*;
import javax.swing.text.Document;

public class JTextAccessorField extends JTextField implements TextAccessor {
    public JTextAccessorField() {
        super();
    }

    public JTextAccessorField(String text) {
        super(text);
    }

    public JTextAccessorField(int columns) {
        super(columns);
    }

    public JTextAccessorField(String text, int columns) {
        super(text, columns);
    }

    public JTextAccessorField(Document doc, String text, int columns) {
        super(doc, text, columns);
    }
}
