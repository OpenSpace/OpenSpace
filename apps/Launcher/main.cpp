/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <QApplication>
#include <QCommandLineParser>

#include <QFile>

#include "mainwindow.h"

#include <ghoul/filesystem/filesystem>

static const QString style = R"style(
QWidget {
    /*font-family: "Helvetica";*/
}
QWidget#MainWindow, QTextEdit, QWidget#SyncWidget, QWidget#DownloadArea {
    background-color: rgb(40, 40, 40);
}
QTextEdit, QLabel, QComboBox, QCheckBox {
    color: #EDEDED;
    font-size: 12px;
}
QLabel {
    font-size: 13px;
}
QLabel#Image {
    margin: -10px 0px -5px 0px;
}
QTextEdit {
    border-width: 2px 2px 0px 0px;
    border-style: solid;
    background-color: rgb(60, 60, 60);
}
QPushButton {
    color:#202020;
    background-color:
        qlineargradient(
            x1: 0, y1: 0, x2: 0, y2: 1,
            stop: 0 white,
            stop: 1 #555555
        );
    border: 1px solid black;
    font-size: 11px;
    min-height: 20px;
}
QComboBox { 
    background-color: rgb(60, 60, 60);
    min-height: 20px;
}
QComboBox:focus, QComboBox:focus QAbstractItemView {
    color: white;
    background-color: rgb(60, 60, 60);
    selection-background-color: rgb(75, 75, 75);
}
QCheckBox {
    border: none;
}
QCheckBox::indicator {
    width: 12px;
    height: 12px;
}
QCheckBox::indicator::unchecked {
    border: 1px solid #5A5A5A;
    background: transparent;
}
QCheckBox::indicator:unchecked:hover {
    border: 1px solid #DDDDDD;
}
QCheckBox::indicator::checked {
    border: 1px solid #AAAAAA;
    background: #666666;
}
QCheckBox::indicator:checked:hover {
    border: 1px solid #DDDDDD;
    background: #555555;
}
QGroupBox, QScrollArea {
    border: 0px;
}
InfoWidget {
    border-width: 1px;
    border-style: solid;
    border-color: #BBBBBB;
    margin: 2px 1px 2px 1px;
    padding: 7.5px;
}
InfoWidget QLabel#Name {
    font-size: 17px;
}
InfoWidget QLabel#Bytes {
    font-size: 13px;
    font-family: "Lucida Console";
} 
InfoWidget QLabel#MessageLeft, QLabel#MessageCenter, QLabel#MessageRight {
    font-size: 11.5px;
    margin-top: -2px;
} 
InfoWidget QProgressBar {
    border: 2px solid #BBBBBB;
    border-radius: 5px;
    background: white;
    height: 15px;
}
InfoWidget QProgressBar::chunk {
    background: qlineargradient(
        x1: 0, y1: 0.5, x2: 1, y2: 0.5,
        stop: 0 #444444,
        stop: 1 #600000
    );
}
QScrollBar {
    border: 1px solid #000000;
    background: #282828;
    width: 15px;
    margin: 16px 0px 16px 0px;
}
QScrollBar::handle {
    background: #B0B0B0;
    border: 1px solid #000000;
    border-width: 1px 0px 1px 0px;
    min-height: 20px;
}
QScrollBar::add-line, QScrollBar::sub-line {
    background:#B0B0B0;
    border: 1px solid #5A5A5A;
    subcontrol-origin: margin;
}
QScrollBar::add-line {
    top: 15px;
    height: 15px;
}
QScrollBar::sub-line {
    height: 15px;
    subcontrol-position: top;
}
QScrollBar::up-arrow, QScrollBar::down-arrow {
    border: 1px solid #5A5A5A;
    width: 3px;
    height: 3px;
    background-color: #353535;
}
QScrollBar::add-page, QScrollBar::sub-page {
    background: none;
}
)style";


int main(int argc, char** argv) {
    QApplication app(argc, argv);
    app.setStyleSheet(style);
    
    MainWindow window;
    window.show();

    return app.exec();
}
