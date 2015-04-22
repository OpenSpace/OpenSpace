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

#include "configurationwidget.h"
#include "timecontrolwidget.h"
#include "informationwidget.h"
#include "timelinewidget.h"

#include <QGridLayout>
#include <QPushButton>
#include <QTextEdit>

#include <array>
#include <cstdint>

MainWindow::MainWindow()
    : QWidget(nullptr)
    , _configurationWidget(nullptr)
    , _timeControlWidget(nullptr)
    , _informationWidget(nullptr)
    , _timelineWidget(nullptr)
	, _socket(nullptr)
{
	setWindowTitle("OpenSpace Timeline");

    _configurationWidget = new ConfigurationWidget(this);
    _timeControlWidget = new TimeControlWidget(this);
    _informationWidget = new InformationWidget(this);
    _timelineWidget = new TimelineWidget(this);

	QGridLayout* layout = new QGridLayout;
    layout->addWidget(_configurationWidget, 0, 0);
    layout->addWidget(_timeControlWidget, 1, 0);
    layout->addWidget(_informationWidget, 2, 0);
    layout->addWidget(_timelineWidget, 0, 1, 3, 1);


    QObject::connect(
        _configurationWidget, SIGNAL(connect(QString, QString)),
        this, SLOT(onConnect(QString, QString))
    );

    QObject::connect(
        _timeControlWidget, SIGNAL(scriptActivity(QString)),
        this, SLOT(sendScript(QString))
    );


	setLayout(layout);
}

MainWindow::~MainWindow() {
	delete _socket;
}

void MainWindow::onConnect(QString host, QString port) {
    delete _socket;

    _socket = new QTcpSocket(this);
    connect(_socket, SIGNAL(readyRead()), SLOT(readTcpData()));
    _socket->connectToHost(host, port.toUInt());
}


void MainWindow::readTcpData() {
    static const uint8_t MessageTypeStatus = 0;

    QByteArray data = _socket->readAll();

    if (QString(data) == "Connected to SGCT!\r\n")
        return;
    if (QString(data) == "OK\r\n")
        return;

    uint8_t messageType = data[0];

    if (messageType == MessageTypeStatus)
        handleStatusMessage(data.mid(1));
        handleStatusMessage(data.right(data.length() - 1));
}

void MainWindow::handleStatusMessage(QByteArray data) {
    const char* buffer = data.data();

    union {
        double value;
        std::array<char, 8> buffer;
    } et;
    std::memmove(et.buffer.data(), buffer, sizeof(double));

    std::vector<char> timeString(24);
    std::memmove(timeString.data(), buffer + sizeof(double), 24);

    union {
        double value;
        std::array<char, 8> buffer;
    } delta;
    std::memmove(delta.buffer.data(), buffer + sizeof(double) + 24, sizeof(double));

    _timeControlWidget->update(
        QString::fromStdString(std::string(timeString.begin(), timeString.end())),
        QString::number(delta.value)
    );
}

void MainWindow::sendScript(QString script) {
    _socket->write(("0" + script + "\r\n").toLatin1());
}
