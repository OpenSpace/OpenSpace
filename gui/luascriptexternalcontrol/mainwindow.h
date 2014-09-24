/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef __MAINWINDOW_H__
#define __MAINWINDOW_H__

#include <QWidget>
#include <QLineEdit>
#include <QTcpSocket>
#include <QTextEdit>

class MainWidget : public QWidget {
Q_OBJECT
public:
	MainWidget();
	~MainWidget();

private slots:
	void onConnectButton();
	void sendCommandButton();
	void readTcpData();

private:
	QLineEdit* _ipAddress;
	QLineEdit* _command;
	QTextEdit* _logWindow;

	QTcpSocket* _socket;


//
//private slots:
//    void on__connectButton_clicked();
//    void on__statsCheckBox_toggled(bool checked);
//    void readTcpData();
//    void on__graphCheckBox_toggled(bool checked);
//    void on__renderComboBox_currentIndexChanged(const QString &arg1);
//
//private:
//    qint64 sendToSGCT(QByteArray data);
//
//    Ui::MainWindow *ui;
//    QTcpSocket* _sgctSocket;
};

#endif // __MAINWINDOW_H__
