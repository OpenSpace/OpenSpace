/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "mainwindow.h"

static const QString style = R"style(
QWidget {
	background-color: rgb(80, 80, 80);
	font-family: Helvetica;
}

QGroupBox {
    background-color: qlineargradient(
    	x1: 0, y1: 0, x2: 0, y2: 1,
    	stop: 0 #858585,
    	stop: 1 #959595);
    border: 2px solid gray;
    border-radius: 5px;
    margin-top: 4ex;
    font-size: bold 12px;
}

QGroupBox::title {
	background-color: #E0E0E0;
    border: 2px solid gray;
    border-radius: 5px;
    subcontrol-origin: margin;
    subcontrol-position: top center;
    padding: 0 10px;
}

QLineEdit {
    color: lightgray;
}

QSlider::groove:horizontal {
    border: 1px solid #999999;
    height: 8px; /* the groove expands to the size of the slider by default. by giving it a height, it has a fixed size */
    background: qlineargradient(
    	x1:0, y1:0, x2:1, y2:0,
    	stop:0 #c4c4c4,
    	stop:0.5 #555555,
    	stop:1 #c4c4c4
    );
    margin: 2px 0;
}

QSlider::handle:horizontal {
    background: qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #b4b4b4, stop:1 #8f8f8f);
    border: 1px solid #5c5c5c;
    width: 18px;
    margin: -2px 0; /* handle is placed by default on the contents rect of the groove. Expand outside the groove */
    border-radius: 3px;
}

QPushButton {
	background-color: lightgray;
    border-style: outset;
    border-width: 0.5px;
    border-radius: 5px;
    border-color: black;
    font: bold 12px;
    min-width: 10em;
}

QPushButton#connection {
	background-color: lightgreen;
}

QPushButton#connection:pressed {
	background-color: green;
}


QPushButton#pause, QPushButton#play {
	padding: 5px;
}

QPushButton#pause:pressed, QPushButton#play:pressed, QPushButton:pressed {
    background-color: darkgray;
    border-style: inset;
}

QCombobox {
    border: 1px solid gray;
    border-radius: 3px;
    padding: 1px 18px 1px 3px;
    min-width: 6em;
}

QComboBox:editable {
    background: lightgrey;	
}

QComboBox QAbstractItemView {
    border: 2px solid darkgray;
    border-radius: 5px;
    background-color: #a8a8a8;
    selection-background-color: #a8a8a8;
}

QLabel#label {
	font-size: 13px;
	background-color: transparent;
	font-variant: small-caps;
}

QLabel#value {
	font-family: monospace;
	font-weight: bold;
	font-size: 14px;
	background-color: transparent;
}

QWidget#background {
	background-color: transparent;
}

QTextEdit {
	font-family: monospace;
}
)style";

int main(int argc, char** argv) {
	QApplication app(argc, argv);

    app.setStyleSheet(style);

	MainWindow window;
	window.show();

	return app.exec();
}
