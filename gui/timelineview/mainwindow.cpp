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
#include "controlwidget.h"
#include "informationwidget.h"
#include "timelinewidget.h"

#include <QGridLayout>
#include <QPushButton>
#include <QTextEdit>

#include <array>
#include <cstdint>

template <typename T>
T readFromBuffer(char* buffer, size_t& currentReadLocation) {
    union {
        T value;
        std::array<char, sizeof(T)> data;
    } b;
    std::memmove(b.data.data(), buffer + currentReadLocation, sizeof(T));
    currentReadLocation += sizeof(T);
    return b.value;
}

template <>
std::string readFromBuffer(char* buffer, size_t& currentReadLocation) {
    uint8_t size = readFromBuffer<uint8_t>(buffer, currentReadLocation);

    std::string result(buffer + currentReadLocation, buffer + currentReadLocation + size);
    currentReadLocation += size;
    return result;
}



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
    _timeControlWidget = new ControlWidget(this);
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

    _configurationWidget->socketDisconnected();
    _timeControlWidget->socketDisconnected();
    _informationWidget->socketDisconnected();
    _timelineWidget->socketDisconnected();
}

MainWindow::~MainWindow() {
	delete _socket;
}

void MainWindow::onConnect(QString host, QString port) {
    delete _socket;

    _socket = new QTcpSocket(this);
    QObject::connect(_socket, SIGNAL(readyRead()), SLOT(readTcpData()));
    QObject::connect(_socket, SIGNAL(connected()), SLOT(onSocketConnected()));
    QObject::connect(_socket, SIGNAL(disconnected()), SLOT(onSocketDisconnected()));

    _socket->connectToHost(host, port.toUInt());
}


void MainWindow::readTcpData() {
    static const uint16_t MessageTypeStatus = 0;
    static const uint16_t MessageTypePlayBook = 2;

    QByteArray data = _socket->readAll();

    //QString debug(data);
    //qDebug() << debug;

    if (QString(data) == "Connected to SGCT!\r\n")
        return;
    if (QString(data) == "OK\r\n")
        return;

    QByteArray messageTypeData = data.left(2);
    union {
        uint16_t value;
        std::array<char, 2> data;
    } messageType;
    std::memcpy(messageType.data.data(), messageTypeData.data(), sizeof(uint16_t));

    switch (messageType.value) {
    case MessageTypeStatus:
        handleStatusMessage(data.mid(2));
        break;
    case MessageTypePlayBook:
    {
        const char* payloadDebug = data.mid(2).data();

        size_t beginning = 0;
        uint32_t size = readFromBuffer<uint32_t>(data.mid(2).data(), beginning);

        while (_socket->waitForReadyRead() && data.size() < size) {
        //while (data.size() < size) {
            data = data.append(_socket->readAll());
        }
        handlePlaybook(data.mid(2));
        break;
    }
    default:
        qDebug() << "Unknown message of type '" << messageType.value << "'";
    }
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
    _timelineWidget->setCurrentTime(std::string(timeString.begin(), timeString.end()), et.value);
}

std::vector<std::string> instrumentsFromId(uint16_t instrumentId, std::map<uint16_t, std::string> instrumentMap) {
    std::vector<std::string> results;
    for (int i = 0; i < 16; ++i) {
        uint16_t testValue = 1 << i;
        if ((testValue & instrumentId) == 1)
            results.push_back(instrumentMap[testValue]);
    }
    return results;
}

void MainWindow::handlePlaybook(QByteArray data) {
    char* buffer = data.data();
    size_t currentReadLocation = 0;

    uint32_t totalData = readFromBuffer<uint32_t>(buffer, currentReadLocation);

    uint8_t nTargets = readFromBuffer<uint8_t>(buffer, currentReadLocation);
    std::map<uint8_t, std::string> targetMap;
    for (uint8_t i = 0; i < nTargets; ++i) {
        uint8_t id = readFromBuffer<uint8_t>(buffer, currentReadLocation);
        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
        targetMap[id] = value;
    }

    uint8_t nInstruments = readFromBuffer<uint8_t>(buffer, currentReadLocation);
    std::map<uint16_t, std::string> instrumentMap;
    for (uint8_t i = 0; i < nInstruments; ++i) {
        uint8_t id = readFromBuffer<uint16_t>(buffer, currentReadLocation);
        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
        instrumentMap[id] = value;
    }

    uint32_t nImages = readFromBuffer<uint32_t>(buffer, currentReadLocation);
    std::vector<Image> images;
    for (uint32_t i = 0; i < nImages; ++i) {
        Image image;
        image.beginning = readFromBuffer<double>(buffer, currentReadLocation);
        image.ending = readFromBuffer<double>(buffer, currentReadLocation);

        image.beginningString = readFromBuffer<std::string>(buffer, currentReadLocation);
        image.endingString = readFromBuffer<std::string>(buffer, currentReadLocation);
        
        uint8_t targetId = readFromBuffer<uint8_t>(buffer, currentReadLocation);
        uint16_t instrumentId = readFromBuffer<uint16_t>(buffer, currentReadLocation);
        image.target = targetMap[targetId];
        image.instruments = instrumentsFromId(instrumentId, instrumentMap);
        images.push_back(image);
    }

    _timelineWidget->setData(std::move(images), std::move(targetMap), std::move(instrumentMap));

    _configurationWidget->socketConnected();
    _timeControlWidget->socketConnected();
    _informationWidget->socketConnected();
    _timelineWidget->socketConnected();
}

void MainWindow::sendScript(QString script) {
    if (_socket)
        _socket->write(("0" + script + "\r\n").toLatin1());
}

void MainWindow::onSocketConnected() {
    _socket->write(QString("1\r\n").toLatin1());
}

void MainWindow::onSocketDisconnected() {
    _configurationWidget->socketDisconnected();
    _timeControlWidget->socketDisconnected();
    _informationWidget->socketDisconnected();
    _timelineWidget->socketDisconnected();
}

