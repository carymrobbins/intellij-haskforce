package com.haskforce.settings;

import com.haskforce.cabal.CabalFileType;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.roots.JavaModuleExternalPaths;
import com.intellij.openapi.roots.ui.CellAppearanceEx;
import com.intellij.openapi.roots.ui.FileAppearanceService;
import com.intellij.openapi.roots.ui.configuration.ModuleConfigurationState;
import com.intellij.openapi.roots.ui.configuration.ModuleElementsEditor;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.ui.AnActionButton;
import com.intellij.ui.AnActionButtonRunnable;
import com.intellij.ui.ColoredTableCellRenderer;
import com.intellij.ui.TableUtil;
import com.intellij.ui.ToolbarDecorator;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.table.JBTable;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ui.ItemRemovable;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.util.List;

/**
 * Tab in Project Settings->Modules.
 */
public class CabalFilesEditor extends ModuleElementsEditor {
    private JTable myTable;

    public static final String NAME = "Cabal Files";

    public CabalFilesEditor(final ModuleConfigurationState state) {
        super(state);
    }

    @Override
    public String getHelpTopic() {
        return NAME;
    }

    @Override
    public String getDisplayName() {
        return NAME;
    }

    @Override
    public void saveData() {
        TableUtil.stopEditing(myTable);
        final int count = myTable.getRowCount();
        String[] urls = ArrayUtil.newStringArray(count);
        for (int row = 0; row < count; row++) {
            final TableItem item = ((MyTableModel)myTable.getModel()).getTableItemAt(row);
            urls[row] = item.getUrl();
        }
        getModel().getModuleExtension(JavaModuleExternalPaths.class).setExternalAnnotationUrls(urls);
    }

    @Override
    public JComponent createComponentImpl() {
        final DefaultTableModel tableModel = createModel();
        myTable = new JBTable(tableModel);
        myTable.setIntercellSpacing(new Dimension(0, 0));
        myTable.setDefaultRenderer(TableItem.class, new MyRenderer());
        myTable.setShowGrid(false);
        myTable.setDragEnabled(false);
        myTable.setShowHorizontalLines(false);
        myTable.setShowVerticalLines(false);
        myTable.getSelectionModel().setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

        JPanel tablePanel = ToolbarDecorator.createDecorator(myTable)
                .setAddAction(new AnActionButtonRunnable() {
                    @Override
                    public void run(AnActionButton button) {
                        FileChooserDescriptor myDescriptor = FileChooserDescriptorFactory.createSingleFileDescriptor(CabalFileType.INSTANCE);
                        myDescriptor.setTitle("Cabal Files");
                        myDescriptor.setDescription("Select Cabal files for module");
                        VirtualFile[] files = FileChooser.chooseFiles(myDescriptor, myTable, myProject, null);
                        final MyTableModel tableModel = (MyTableModel)myTable.getModel();
                        boolean changes = false;
                        for (final VirtualFile file : files) {
                            if (file != null) {
                                tableModel.addTableItem(new TableItem(file));
                                changes = true;
                            }
                        }
                        if (changes) {
                            saveData();
                            TableUtil.selectRows(myTable, new int[] {tableModel.getRowCount() - 1});
                        }
                    }
                }).setRemoveAction(new AnActionButtonRunnable() {
                    @Override
                    public void run(AnActionButton button) {
                        final List<Object[]> removedItems = TableUtil.removeSelectedItems(myTable);
                        if (!removedItems.isEmpty()) {
                            saveData();
                        }
                    }
                }).createPanel();


        final JPanel mainPanel = new JPanel(new BorderLayout());

        mainPanel.add(tablePanel, BorderLayout.CENTER);
        mainPanel.add(new JBLabel("Cabal files for module", UIUtil.ComponentStyle.SMALL,
                UIUtil.FontColor.BRIGHTER), BorderLayout.NORTH);
        return mainPanel;
    }

    protected DefaultTableModel createModel() {
        final MyTableModel tableModel = new MyTableModel();
        final String[] urls = getModel().getModuleExtension(JavaModuleExternalPaths.class).getExternalAnnotationsUrls();
        for (String javadocUrl : urls) {
            tableModel.addTableItem(new TableItem(javadocUrl));
        }
        return tableModel;
    }

    @Override
    public void moduleStateChanged() {
        if (myTable != null) {
            final DefaultTableModel tableModel = createModel();
            myTable.setModel(tableModel);
        }
    }

    private static class MyRenderer extends ColoredTableCellRenderer {
        private static final Border NO_FOCUS_BORDER = BorderFactory.createEmptyBorder(1, 1, 1, 1);

        @Override
        protected void customizeCellRenderer(JTable table, Object value, boolean selected, boolean hasFocus, int row, int column) {
            setPaintFocusBorder(false);
            setFocusBorderAroundIcon(true);
            setBorder(NO_FOCUS_BORDER);

            final TableItem tableItem = (TableItem) value;
            tableItem.getCellAppearance().customize(this);
        }
    }

    private static class MyTableModel extends DefaultTableModel implements ItemRemovable{
        @Override
        public String getColumnName(int column) {
            return null;
        }

        @Override
        public Class getColumnClass(int columnIndex) {
            return TableItem.class;
        }

        @Override
        public int getColumnCount() {
            return 1;
        }

        @Override
        public boolean isCellEditable(int row, int column) {
            return false;
        }

        public TableItem getTableItemAt(int row) {
            return (TableItem)getValueAt(row, 0);
        }

        public void addTableItem(TableItem item) {
            addRow(new Object[] {item});
        }
    }

    /**
     * Copy and paste class because of protected access.
     */
    private static class TableItem {
        private final String myUrl;
        private final CellAppearanceEx myCellAppearance;

        public TableItem(@NotNull final VirtualFile file) {
            myUrl = file.getUrl();
            myCellAppearance = FileAppearanceService.getInstance().forVirtualFile(file);
        }

        public TableItem(@NotNull final String url) {
            myUrl = url;

            final VirtualFile file = VirtualFileManager.getInstance().findFileByUrl(url);
            if (file != null) {
                myCellAppearance = FileAppearanceService.getInstance().forVirtualFile(file);
            }
            else {
                myCellAppearance = FileAppearanceService.getInstance().forInvalidUrl(url);
            }
        }

        @NotNull
        public String getUrl() {
            return myUrl;
        }

        @NotNull
        public CellAppearanceEx getCellAppearance() {
            return myCellAppearance;
        }
    }
}
