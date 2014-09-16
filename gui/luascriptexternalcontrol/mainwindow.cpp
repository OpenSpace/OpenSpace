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

#include "mainwindow.h"

#include <QGridLayout>
#include <QPushButton>
#include <QTextEdit>

MainWidget::MainWidget()
	: _ipAddress(new QLineEdit)
	, _command(new QLineEdit)
	, _logWindow(new QTextEdit)
	, _socket(nullptr)
{
	setWindowTitle("OpenSpace LuaScripting GUI");

	QGridLayout* layout = new QGridLayout;
	_ipAddress->setMinimumWidth(200);
	_ipAddress->setText("127.0.0.1");
	layout->addWidget(_ipAddress, 0, 0);

	QPushButton* connectButton = new QPushButton("Connect");
	connect(connectButton, SIGNAL(clicked()), this, SLOT(onConnectButton()));
	connectButton->show();
	layout->addWidget(connectButton, 0, 1);

	_command->setMinimumWidth(200);
	layout->addWidget(_command, 1, 0);

	QPushButton* sendButton = new QPushButton("Send");
	sendButton->setDefault(true);
	connect(sendButton, SIGNAL(clicked()), this, SLOT(sendCommandButton()));
	layout->addWidget(sendButton, 1, 1);

	layout->addWidget(_logWindow, 2, 0, 1, 2);

	setLayout(layout);
}

MainWidget::~MainWidget() {
	delete _command;
	delete _socket;
}

void MainWidget::readTcpData() {
    QByteArray data = _socket->readAll();

	if (_logWindow->toPlainText().isEmpty())
		_logWindow->setText(data.data());
	else
		_logWindow->setText(_logWindow->toPlainText() + "\n" + data.data());
}

void MainWidget::onConnectButton() {
	delete _socket;
	
	_socket = new QTcpSocket(this);
    connect( _socket, SIGNAL(readyRead()), SLOT(readTcpData()) );
    _socket->connectToHost(_ipAddress->text(), 20500);

}

void MainWidget::sendCommandButton() {
	if (!_socket) {
		if (_logWindow->toPlainText().isEmpty())
			_logWindow->setText("No connection found");
		else
			_logWindow->setText(_logWindow->toPlainText() + "\n" + "No connection found");
		return;
	}

	QString command = _command->text();

	if (_logWindow->toPlainText().isEmpty())
		_logWindow->setText(command);
	else
		_logWindow->setText(_logWindow->toPlainText() + "\n" + command);

	
	_socket->write(("0" + command + "\r\n").toLatin1());
}
