#!/usr/bin/env python

# requires python3-pyqt5

from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtPrintSupport import *

import os
import sys


class MainWindow(QMainWindow):
    def __init__(self, *args, **kwargs):
        super(MainWindow, self).__init__(*args, **kwargs)

        layout = QVBoxLayout()
        self.editor = QPlainTextEdit()

        fixedfont = QFontDatabase.systemFont(QFontDatabase.FixedFont)
        fixedfont.setPointSize(12)
        self.editor.setFont(fixedfont)

        self.path = None

        layout.addWidget(self.editor)

        container = QWidget()
        container.setLayout(layout)
        self.setCentralWidget(container)

        self.status = QStatusBar()
        self.setStatusBar(self.status)

        file_toolbar = QToolBar("File")
        file_toolbar.setIconSize(QSize(14, 14))
        self.addToolBar(file_toolbar)

        open_file_action = QAction("Open file", self)
        open_file_action.triggered.connect(self.file_open)
        file_toolbar.addAction(open_file_action)

        save_file_action = QAction("Save", self)
        save_file_action.triggered.connect(self.file_save)
        file_toolbar.addAction(save_file_action)

        saveas_file_action = QAction("Save As", self)
        saveas_file_action.triggered.connect(self.file_saveas)
        file_toolbar.addAction(saveas_file_action)

        print_action = QAction("Print", self)
        print_action.triggered.connect(self.file_print)
        file_toolbar.addAction(print_action)

        quit_action = QAction("Quit", self)
        quit_action.triggered.connect(self.file_quit)
        file_toolbar.addAction(quit_action)

        edit_toolbar = QToolBar("Edit")
        edit_toolbar.setIconSize(QSize(16, 16))
        self.addToolBar(edit_toolbar)

        undo_action = QAction("Undo", self)
        undo_action.triggered.connect(self.editor.undo)
        edit_toolbar.addAction(undo_action)

        redo_action = QAction("Redo", self)
        redo_action.triggered.connect(self.editor.redo)
        edit_toolbar.addAction(redo_action)

        cut_action = QAction("Cut", self)
        cut_action.triggered.connect(self.editor.cut)
        edit_toolbar.addAction(cut_action)

        copy_action = QAction("Copy", self)
        copy_action.triggered.connect(self.editor.copy)
        edit_toolbar.addAction(copy_action)

        paste_action = QAction("Paste", self)
        paste_action.triggered.connect(self.editor.paste)
        edit_toolbar.addAction(paste_action)

        select_action = QAction("Select all", self)
        select_action.triggered.connect(self.editor.selectAll)
        edit_toolbar.addAction(select_action)

        wrap_action = QAction("Wrap", self)
        wrap_action.setCheckable(True)
        wrap_action.setChecked(True)
        wrap_action.triggered.connect(self.edit_toggle_wrap)
        edit_toolbar.addAction(wrap_action)

        self.update_title()
        self.show()

    def dialog_critical(self, s):
        dlg = QMessageBox(self)
        dlg.setText(s)
        dlg.setIcon(QMessageBox.Critical)
        dlg.show()

    def file_open(self):
        path, _ = QFileDialog.getOpenFileName(self, "Open file", "")

        if path:
            try:
                with open(path, 'rU') as f:
                    text = f.read()

            except Exception as e:
                self.dialog_critical(str(e))

            else:
                self.path = path
                self.editor.setPlainText(text)
                self.update_title()

    def file_save(self):
        if self.path is None:
            return self.file_saveas()

        self._save_to_path(self.path)

    def file_saveas(self):
        path, _ = QFileDialog.getSaveFileName(self, "Save file", "",)

        if not path:
            # If dialog is cancelled, will return ''
            return

        self._save_to_path(path)

    def _save_to_path(self, path):
        text = self.editor.toPlainText()
        try:
            with open(path, 'w') as f:
                f.write(text)

        except Exception as e:
            self.dialog_critical(str(e))

        else:
            self.path = path
            self.update_title()

    def file_print(self):
        dlg = QPrintDialog()
        if dlg.exec_():
            self.editor.print_(dlg.printer())

    def file_quit(self):
        self.close()

    def update_title(self):
        self.setWindowTitle(
            "%s" % (os.path.basename(self.path) if self.path else "untitled"))

    def edit_toggle_wrap(self):
        self.editor.setLineWrapMode(
            1 if self.editor.lineWrapMode() == 0 else 0)


if __name__ == '__main__':

    app = QApplication(sys.argv)
    app.setApplicationName("untitled")

    window = MainWindow()
    app.exec_()