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

#include <QFile>

#include "mainwindow.h"

#include <ghoul/filesystem/filesystem>

//	Copyright 2013 Emanuel Claesson
//
//	Licensed under the Apache License, Version 2.0 (the "License");
//	you may not use this file except in compliance with the License.
//	You may obtain a copy of the License at
//
//		http://www.apache.org/licenses/LICENSE-2.0
//
//	Unless required by applicable law or agreed to in writing, software
//	distributed under the License is distributed on an "AS IS" BASIS,
//	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//	See the License for the specific language governing permissions and
//	limitations under the License.
//*/
//
///*
//	COLOR_DARK     = #191919
//	COLOR_MEDIUM   = #353535
//	COLOR_MEDLIGHT = #5A5A5A
//	COLOR_LIGHT    = #DDDDDD
//	COLOR_ACCENT   = #3D7848
//*/
//
//* {
//	background: #191919;
//	color: #DDDDDD;
//	border: 1px solid #5A5A5A;
//}
//
//QWidget::item:selected {
//	background: #3D7848;
//}
//
//QCheckBox, QRadioButton {
//	border: none;
//}
//
//QRadioButton::indicator, QCheckBox::indicator {
//	width: 13px;
//	height: 13px;
//}
//
//QRadioButton::indicator::unchecked, QCheckBox::indicator::unchecked {
//	border: 1px solid #5A5A5A;
//	background: none;
//}
//
//QRadioButton::indicator:unchecked:hover, QCheckBox::indicator:unchecked:hover {
//	border: 1px solid #DDDDDD;
//}
//
//QRadioButton::indicator::checked, QCheckBox::indicator::checked {
//	border: 1px solid #5A5A5A;
//	background: #5A5A5A;
//}
//
//QRadioButton::indicator:checked:hover, QCheckBox::indicator:checked:hover {
//	border: 1px solid #DDDDDD;
//	background: #DDDDDD;
//}
//
//QGroupBox {
//	margin-top: 6px;
//}
//
//QGroupBox::title {
//	top: -7px;
//	left: 7px;
//}
//
//QScrollBar {
//	border: 1px solid #5A5A5A;
//	background: #191919;
//}
//
//QScrollBar:horizontal {
//	height: 15px;
//	margin: 0px 0px 0px 32px;
//}
//
//QScrollBar:vertical {
//	width: 15px;
//	margin: 32px 0px 0px 0px;
//}
//
//QScrollBar::handle {
//	background: #353535;
//	border: 1px solid #5A5A5A;
//}
//
//QScrollBar::handle:horizontal {
//	border-width: 0px 1px 0px 1px;
//}
//
//QScrollBar::handle:vertical {
//	border-width: 1px 0px 1px 0px;
//}
//
//QScrollBar::handle:horizontal {
//	min-width: 20px;
//}
//
//QScrollBar::handle:vertical {
//	min-height: 20px;
//}
//
//QScrollBar::add-line, QScrollBar::sub-line {
//	background:#353535;
//	border: 1px solid #5A5A5A;
//	subcontrol-origin: margin;
//}
//
//QScrollBar::add-line {
//	position: absolute;
//}
//
//QScrollBar::add-line:horizontal {
//	width: 15px;
//	subcontrol-position: left;
//	left: 15px;
//}
//
//QScrollBar::add-line:vertical {
//	height: 15px;
//	subcontrol-position: top;
//	top: 15px;
//}
//
//QScrollBar::sub-line:horizontal {
//	width: 15px;
//	subcontrol-position: top left;
//}
//
//QScrollBar::sub-line:vertical {
//	height: 15px;
//	subcontrol-position: top;
//}
//
//QScrollBar:left-arrow, QScrollBar::right-arrow, QScrollBar::up-arrow, QScrollBar::down-arrow {
//	border: 1px solid #5A5A5A;
//	width: 3px;
//	height: 3px;
//}
//
//QScrollBar::add-page, QScrollBar::sub-page {
//	background: none;
//}
//
//QAbstractButton:hover {
//	background: #353535;
//}
//
//QAbstractButton:pressed {
//	background: #5A5A5A;
//}
//
//QAbstractItemView {
//	show-decoration-selected: 1;
//	selection-background-color: #3D7848;
//	selection-color: #DDDDDD;
//	alternate-background-color: #353535;
//}
//
//QHeaderView {
//	border: 1px solid #5A5A5A;
//}
//
//QHeaderView::section {
//	background: #191919;
//	border: 1px solid #5A5A5A;
//	padding: 4px;
//}
//
//QHeaderView::section:selected, QHeaderView::section::checked {
//	background: #353535;
//}
//
//QTableView {
//	gridline-color: #5A5A5A;
//}
//
//QTabBar {
//	margin-left: 2px;
//}
//
//QTabBar::tab {
//	border-radius: 0px;
//	padding: 4px;
//	margin: 4px;
//}
//
//QTabBar::tab:selected {
//	background: #353535;
//}
//
//QComboBox::down-arrow {
//	border: 1px solid #5A5A5A;
//	background: #353535;
//}
//
//QComboBox::drop-down {
//	border: 1px solid #5A5A5A;
//	background: #353535;
//}
//
//QComboBox::down-arrow {
//	width: 3px;
//	height: 3px;
//	border: 1px solid #5A5A5A;
//}
//
//QAbstractSpinBox {
//	padding-right: 15px;
//}
//
//QAbstractSpinBox::up-button, QAbstractSpinBox::down-button {
//	border: 1px solid #5A5A5A;
//	background: #353535;
//	subcontrol-origin: border;
//}
//
//QAbstractSpinBox::up-arrow, QAbstractSpinBox::down-arrow {
//	width: 3px;
//	height: 3px;
//	border: 1px solid #5A5A5A;
//}
//
//QSlider {
//	border: none;
//}
//
//QSlider::groove:horizontal {
//	height: 5px;
//	margin: 4px 0px 4px 0px;
//}
//
//QSlider::groove:vertical {
//	width: 5px;
//	margin: 0px 4px 0px 4px;
//}
//
//QSlider::handle {
//	border: 1px solid #5A5A5A;
//	background: #353535;
//}
//
//QSlider::handle:horizontal {
//	width: 15px;
//	margin: -4px 0px -4px 0px;
//}
//
//QSlider::handle:vertical {
//	height: 15px;
//	margin: 0px -4px 0px -4px;
//}
//
//QSlider::add-page:vertical, QSlider::sub-page:horizontal {
//	background: #3D7848;
//}
//
//QSlider::sub-page:vertical, QSlider::add-page:horizontal {
//	background: #353535;
//}
//
//QLabel {
//	border: none;
//}
//
//QProgressBar {
//	text-align: center;
//}
//
//QProgressBar::chunk {
//	width: 1px;
//	background-color: #3D7848;
//}
//
//QMenu::separator {
//	background: #353535;
//}
//)style";

//static const QString style = R"style(
//QWidget {
//	font-family: Helvetica;
//}
//QWidget#MainWindow, QTextEdit, QWidget#SyncWidget, QWidget#DownloadArea {
//	background-color: rgb(40, 40, 40);
//}
//QTextEdit, QLabel, QComboBox, QCheckBox {
//    color: white;
//    font-size: 12px;
//}
//QTextEdit {
//    border-width: 2px 2px 0px 0px;
//    border-style: solid;
//	background-color: rgb(60, 60, 60);
//}
//QPushButton {
//    background-color:
//        qlineargradient(
//            x1: 0, y1: 0, x2: 0, y2: 1,
//            stop: 0 white,
//            stop: 1 #505050
//        );
//    border-style: solid;
//    border-color: black;
//    border-width: 1px;
//    font-size: 11px;
//    min-height: 17.5px;
//}
//QComboBox {
//    background-color: rgb(60, 60, 60);
//    min-height: 20px;
//}
//QComboBox:focus, QComboBox:focus QAbstractItemView {
//    color: white;
//    background-color: rgb(60, 60, 60);
//    selection-background-color: rgb(75, 75, 75);
//}
//QCheckBox {
//	border: none;
//}
//QCheckBox::indicator {
//	width: 13px;
//	height: 13px;
//}
//QCheckBox::indicator::unchecked {
//	border: 1px solid #5A5A5A;
//	background: transparent;
//}
//QCheckBox::indicator:unchecked:hover {
//	border: 1px solid #DDDDDD;
//}
//QCheckBox::indicator::checked {
//	border: 1px solid #666666;
//	background: #666666;
//}
//QCheckBox::indicator:checked:hover {
//	border: 1px solid #DDDDDD;
//	background: #555555;
//}
//QGroupBox, QScrollArea {
//    border: 0px;
//}
//)style";

int main(int argc, char** argv) {
	QApplication app(argc, argv);

	MainWindow window;

    auto setStyle = [](const ghoul::filesystem::File& f) {
        QFile file(QString::fromStdString(f.path()));
        file.open(QIODevice::ReadOnly);
        QTextStream in(&file);
        QString style = in.readAll();
        qApp->setStyleSheet(style);
    };

    ghoul::filesystem::File f("stylesheet.css", false, setStyle);

    setStyle(f);

	window.show();

	return app.exec();
}
