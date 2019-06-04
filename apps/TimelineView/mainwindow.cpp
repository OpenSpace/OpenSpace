/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <QThread>

#include <array>
#include <cstdint>

namespace {
    QByteArray continuousData;
}

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
    _configurationWidget->setMinimumWidth(350);
    _timeControlWidget = new ControlWidget(this);
    _timeControlWidget->setMinimumWidth(350);
    _informationWidget = new InformationWidget(this);
    _informationWidget->setMinimumWidth(350);
    _timelineWidget = new TimelineWidget(this);

    QGridLayout* layout = new QGridLayout;
    layout->addWidget(_configurationWidget, 0, 0);
    layout->addWidget(_timeControlWidget, 1, 0);
    layout->addWidget(_informationWidget, 2, 0);
    layout->addWidget(_timelineWidget, 0, 1, 3, 1);

    layout->setColumnStretch(1, 5);


    QObject::connect(
        _configurationWidget, SIGNAL(connect(QString, QString)),
        this, SLOT(onConnect(QString, QString))
    );
    QObject::connect(
        _configurationWidget, SIGNAL(disconnect()),
        this, SLOT(onDisconnect())
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

void MainWindow::onDisconnect() {
    delete _socket;
    _socket = nullptr;
}

void MainWindow::readTcpData() {
    static const uint16_t MessageTypeStatus = 0;
    static const uint16_t MessageTypeMappingIdentifier = 1;
    static const uint16_t MessageTypeInitialMessageFinished = 2;
    static const uint16_t MessageTypePlayBookLabel = 3;
    static const uint16_t MessageTypePlayBookHongKang = 4;

    QByteArray data = continuousData.append(_socket->readAll());
//    int d = data.size();

    if (QString(data) == "Connected to SGCT!\r\n") {
        continuousData.clear();
        return;
    }
    if (QString(data) == "OK\r\n") {
        continuousData.clear();
        return;
    }

    if (data.size() != 42)
        qDebug() << QString(data);

    QByteArray messageTypeData = data.left(2);
    union {
        uint16_t value;
        std::array<char, 2> data;
    } messageType;
    std::memcpy(messageType.data.data(), messageTypeData.data(), sizeof(uint16_t));

    switch (messageType.value) {
    case MessageTypeStatus:
        break;
    case MessageTypeMappingIdentifier:
        qDebug() << "Mapping Identifier received";
        printMapping(data.mid(2));
        continuousData.clear();
        break;
    case MessageTypeInitialMessageFinished:
        qDebug() << "InitialMessageFinished received";
        break;
    case MessageTypePlayBookHongKang:
        qDebug() << "Hong Kang Playbook received";
        break;
    case MessageTypePlayBookLabel:
        qDebug() << "Label Playbook received";
        break;
    default:
        qDebug() << "Unknown message of type '" << messageType.value << "'";
    }

    switch (messageType.value) {
    case MessageTypeStatus:
    {
        if (_isConnected)
            handleStatusMessage(data.mid(2));
        continuousData.clear();
        break;
    }
    case MessageTypePlayBookHongKang:
    case MessageTypePlayBookLabel:
    {
//        const char* payloadDebug = data.mid(2).data();

        size_t beginning = 0;
        uint32_t size = readFromBuffer<uint32_t>(data.mid(2).data(), beginning);

        //qDebug() << "Begin reading data";
        while (_socket->waitForReadyRead() && data.size() < int(size)) {
            //qDebug() << ".";
            //_socket->read
            //data = data.append(_socket->re)
            data = data.append(_socket->readAll());
            //data = data.append(_socket->read(int(size) - data.size()));
            //QThread::msleep(50);
        }
        //qDebug() << "Finished reading data. Handling playbook";

        continuousData = handlePlaybook(data.mid(2));


        //qDebug() << "Finished handling playbook";

        //if (messageType.value == MessageTypePlayBookHongKang)
        //    _hasHongKangTimeline = true;
        //if (messageType.value == MessageTypePlayBookLabel)
        //    _hasLabelTimeline = true;

        //if (_hasHongKangTimeline && _hasLabelTimeline) {
        //    fullyConnected();
        //}

        break;
    }
    case MessageTypeInitialMessageFinished:
        _isConnected = true;
        fullyConnected();
        continuousData.clear();
        break;

    default:
        qDebug() << QString(data);
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
    _timelineWidget->setCurrentTime(
        std::string(timeString.begin(), timeString.end()),
        et.value
    );
}

std::vector<std::string> instrumentsFromId(uint16_t instrumentId,
                                           std::map<uint16_t, std::string> instrumentMap)
{
    std::vector<std::string> results;
    for (int i = 0; i < 16; ++i) {
        uint16_t testValue = 1 << i;
        if ((testValue & instrumentId) != 0) {
            std::string t = instrumentMap.at(testValue);
            if (t.empty()) {
                qDebug() << "Empty instrument";
            }
            results.push_back(t);
        }
    }
    return results;
}

QByteArray MainWindow::handlePlaybook(QByteArray data) {
    char* buffer = data.data();
    size_t currentReadLocation = 0;

    uint32_t totalData = readFromBuffer<uint32_t>(buffer, currentReadLocation);

    uint8_t nTargets = readFromBuffer<uint8_t>(buffer, currentReadLocation);
    qDebug() << "Targets: " << nTargets;
    std::map<uint8_t, std::string> targetMap;
    for (uint8_t i = 0; i < nTargets; ++i) {
        uint8_t id = readFromBuffer<uint8_t>(buffer, currentReadLocation);
        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
        qDebug() << QString::fromStdString(value);
        targetMap[id] = value;
    }

    uint8_t nInstruments = readFromBuffer<uint8_t>(buffer, currentReadLocation);
    qDebug() << "Instruments: " << nInstruments;
    std::map<uint16_t, std::string> instrumentMap;
    for (uint8_t i = 0; i < nInstruments; ++i) {
        uint16_t id = readFromBuffer<uint16_t>(buffer, currentReadLocation);
        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
        qDebug() << QString::fromStdString(value);
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
        if (image.instruments.empty())
            qDebug() << "Instruments were empty";
        images.push_back(image);
    }
    _timelineWidget->setData(
        std::move(images),
        std::move(targetMap),
        std::move(instrumentMap)
    );

    auto dataSize = data.size();
    auto readSize = currentReadLocation;
    auto extraBytes = dataSize - readSize;
    if (extraBytes > 0) {
        return data.mid(currentReadLocation);
    }
    else {
        return QByteArray();
    }
}

void MainWindow::sendScript(QString script) {
    if (_socket) {
        _socket->write(("0" + script + "\r\n").toLatin1());
        //QByteArray data = (QString("0") + script).toLocal8Bit();
        //qDebug() << data;
        //_socket->write(data);
        //QThread::msleep(25);
    }
        //_socket->write(("0" + script + "\r\n").toLatin1());
        //_socket->write(("0" + script + "\0").toLatin1());
}

void MainWindow::onSocketConnected() {
    _socket->write(QString("1\r\n").toLatin1());
    //_socket->write(QString("1").toLatin1());

}

void MainWindow::onSocketDisconnected() {
    _configurationWidget->socketDisconnected();
    _timeControlWidget->socketDisconnected();
    _informationWidget->socketDisconnected();
    _timelineWidget->socketDisconnected();

    _informationWidget->logInformation("Disconnected.");
}

std::string MainWindow::nextTarget() const {
    return _timelineWidget->nextTarget();
}

void MainWindow::fullyConnected() {
    _informationWidget->logInformation(
        "Connected to " + _socket->peerName() + " on port " +
        QString::number(_socket->peerPort()) + "."
    );

    _configurationWidget->socketConnected();
    _timeControlWidget->socketConnected();
    _informationWidget->socketConnected();
    _timelineWidget->socketConnected();
}

void MainWindow::printMapping(QByteArray data) {
    char* buffer = data.data();
    size_t currentReadPosition = 0;

    uint16_t size = readFromBuffer<uint16_t>(buffer, currentReadPosition);
    for (uint16_t i = 0; i < size; ++i) {
        uint16_t identifier = readFromBuffer<uint16_t>(buffer, currentReadPosition);
        std::string mapping = readFromBuffer<std::string>(buffer, currentReadPosition);

        qDebug() << identifier << ": " << QString::fromStdString(mapping);
    }
}
