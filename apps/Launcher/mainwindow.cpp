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

#include "mainwindow.h"

#include "informationwidget.h"

#include <QComboBox>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QThread>

namespace {
    const QSize WindowSize = QSize(640, 480);
}

MainWindow::MainWindow()
    : QWidget(nullptr)
    , _informationWidget(nullptr)
{
    setFixedSize(WindowSize);
    
    QGridLayout* layout = new QGridLayout;
    
    QLabel* image = new QLabel;
    QPixmap p = QPixmap(":/images/header.png");
    image->setPixmap(p.scaledToWidth(WindowSize.width()));
    layout->addWidget(image, 0, 0, 1, 2);
    
    _informationWidget = new InformationWidget(this);
    layout->addWidget(_informationWidget, 1, 0, 2, 1);

    QWidget* container = new QWidget;
    {
        QGridLayout* layout = new QGridLayout;
        
        QLabel* shortcutLabel = new QLabel("Keyboard Shortcuts:");
        layout->addWidget(shortcutLabel, 0, 0);
        QPushButton* shortcutButton = new QPushButton("Open...");
        QObject::connect(shortcutButton, SIGNAL(clicked(bool)),
                         this, SLOT(shortcutButtonPressed())
                         );
        layout->addWidget(shortcutButton, 0, 1);
        
        QLabel* sceneSelectionLabel = new QLabel("Scenes:");
        layout->addWidget(sceneSelectionLabel, 1, 0);
        QComboBox* sceneSelection = new QComboBox;
        layout->addWidget(sceneSelection);
        
        container->setLayout(layout);
    }
    layout->addWidget(container, 1, 1);
    
    container = new QWidget;
    {
        QBoxLayout* layout = new QHBoxLayout;
        
        QPushButton* cancelButton = new QPushButton("Cancel");
        layout->addWidget(cancelButton);
        
        QPushButton* syncButton = new QPushButton("Sync");
        layout->addWidget(syncButton);
        
        QPushButton* startButton = new QPushButton("Start");
        layout->addWidget(startButton);
        
        container->setLayout(layout);
    }
    layout->addWidget(container, 2, 1);
    
    setLayout(layout);
}

MainWindow::~MainWindow() {
    delete _informationWidget;
}

void MainWindow::shortcutButtonPressed() {
    
}

//MainWindow::MainWindow()
//    : QWidget(nullptr)
//    , _configurationWidget(nullptr)
//    , _timeControlWidget(nullptr)
//    , _informationWidget(nullptr)
//    , _timelineWidget(nullptr)
//	, _socket(nullptr)
//{
//	setWindowTitle("OpenSpace Timeline");
//
//    _configurationWidget = new ConfigurationWidget(this);
//    _configurationWidget->setMinimumWidth(350);
//    _timeControlWidget = new ControlWidget(this);
//    _timeControlWidget->setMinimumWidth(350);
//    _informationWidget = new InformationWidget(this);
//    _informationWidget->setMinimumWidth(350);
//    _timelineWidget = new TimelineWidget(this);
//
//	QGridLayout* layout = new QGridLayout;
//    layout->addWidget(_configurationWidget, 0, 0);
//    layout->addWidget(_timeControlWidget, 1, 0);
//    layout->addWidget(_informationWidget, 2, 0);
//    layout->addWidget(_timelineWidget, 0, 1, 3, 1);
//
//    layout->setColumnStretch(1, 5);
//
//
//    QObject::connect(
//        _configurationWidget, SIGNAL(connect(QString, QString)),
//        this, SLOT(onConnect(QString, QString))
//    );
//    QObject::connect(
//        _configurationWidget, SIGNAL(disconnect()),
//        this, SLOT(onDisconnect())
//    );
//
//    QObject::connect(
//        _timeControlWidget, SIGNAL(scriptActivity(QString)),
//        this, SLOT(sendScript(QString))
//    );
//
//	setLayout(layout);
//
//    _configurationWidget->socketDisconnected();
//    _timeControlWidget->socketDisconnected();
//    _informationWidget->socketDisconnected();
//    _timelineWidget->socketDisconnected();
//}
//
//MainWindow::~MainWindow() {
//	delete _socket;
//}
//
//void MainWindow::onConnect(QString host, QString port) {
//    delete _socket;
//
//    _socket = new QTcpSocket(this);
//    QObject::connect(_socket, SIGNAL(readyRead()), SLOT(readTcpData()));
//    QObject::connect(_socket, SIGNAL(connected()), SLOT(onSocketConnected()));
//    QObject::connect(_socket, SIGNAL(disconnected()), SLOT(onSocketDisconnected()));
//
//    _socket->connectToHost(host, port.toUInt());
//}
//
//void MainWindow::onDisconnect() {
//    delete _socket;
//    _socket = nullptr;
//}
//
//void MainWindow::readTcpData() {
//    static const uint16_t MessageTypeStatus = 0;
//    static const uint16_t MessageTypePlayBookHongKang = 2;
//    static const uint16_t MessageTypePlayBookLabel = 3;
//
//    QByteArray data = _socket->readAll();
//
//    if (QString(data) == "Connected to SGCT!\r\n")
//        return;
//    if (QString(data) == "OK\r\n")
//        return;
//
//    QByteArray messageTypeData = data.left(2);
//    union {
//        uint16_t value;
//        std::array<char, 2> data;
//    } messageType;
//    std::memcpy(messageType.data.data(), messageTypeData.data(), sizeof(uint16_t));
//
//    switch (messageType.value) {
//    case MessageTypeStatus:
//        break;
//    case MessageTypePlayBookHongKang:
//        qDebug() << "Hong Kang Playbook received";
//        break;
//    case MessageTypePlayBookLabel:
//        qDebug() << "Label Playbook received";
//        break;
//    default:
//        qDebug() << "Unknown message of type '" << messageType.value << "'";
//    }
//
//    switch (messageType.value) {
//    case MessageTypeStatus:
//    {
//        if (_hasHongKangTimeline && _hasLabelTimeline)
//            handleStatusMessage(data.mid(2));
//        break;
//    }
//    case MessageTypePlayBookHongKang:
//    case MessageTypePlayBookLabel:
//    {
//        const char* payloadDebug = data.mid(2).data();
//
//        size_t beginning = 0;
//        uint32_t size = readFromBuffer<uint32_t>(data.mid(2).data(), beginning);
//
//        //qDebug() << "Begin reading data";
//        while (_socket->waitForReadyRead() && data.size() < int(size)) {
//            //qDebug() << ".";
//            data = data.append(_socket->readAll());
//            //data = data.append(_socket->read(int(size) - data.size()));
//            QThread::msleep(50);
//        }
//        //qDebug() << "Finished reading data. Handling playbook";
//
//        handlePlaybook(data.mid(2));
//
//        //qDebug() << "Finished handling playbook";
//
//        if (messageType.value == MessageTypePlayBookHongKang)
//            _hasHongKangTimeline = true;
//        if (messageType.value == MessageTypePlayBookLabel)
//            _hasLabelTimeline = true;
//
//        if (_hasHongKangTimeline && _hasLabelTimeline) {
//            fullyConnected();
//        }
//
//        break;
//    }
//    default:
//        qDebug() << QString(data);
//    }
//
//}
//
//void MainWindow::handleStatusMessage(QByteArray data) {
//    const char* buffer = data.data();
//
//    union {
//        double value;
//        std::array<char, 8> buffer;
//    } et;
//    std::memmove(et.buffer.data(), buffer, sizeof(double));
//
//    std::vector<char> timeString(24);
//    std::memmove(timeString.data(), buffer + sizeof(double), 24);
//
//    union {
//        double value;
//        std::array<char, 8> buffer;
//    } delta;
//    std::memmove(delta.buffer.data(), buffer + sizeof(double) + 24, sizeof(double));
//
//    _timeControlWidget->update(
//        QString::fromStdString(std::string(timeString.begin(), timeString.end())),
//        QString::number(delta.value)
//    );
//    _timelineWidget->setCurrentTime(std::string(timeString.begin(), timeString.end()), et.value);
//}
//
//std::vector<std::string> instrumentsFromId(uint16_t instrumentId, std::map<uint16_t, std::string> instrumentMap) {
//    std::vector<std::string> results;
//    for (int i = 0; i < 16; ++i) {
//        uint16_t testValue = 1 << i;
//        if ((testValue & instrumentId) != 0) {
//            std::string t = instrumentMap.at(testValue);
//            if (t.empty())
//                qDebug() << "Empty instrument";
//            results.push_back(t);
//        }
//    }
//    return results;
//}
//
//void MainWindow::handlePlaybook(QByteArray data) {
//    char* buffer = data.data();
//    size_t currentReadLocation = 0;
//
//    uint32_t totalData = readFromBuffer<uint32_t>(buffer, currentReadLocation);
//
//    uint8_t nTargets = readFromBuffer<uint8_t>(buffer, currentReadLocation);
//    qDebug() << "Targets: " << nTargets;
//    std::map<uint8_t, std::string> targetMap;
//    for (uint8_t i = 0; i < nTargets; ++i) {
//        uint8_t id = readFromBuffer<uint8_t>(buffer, currentReadLocation);
//        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
//        qDebug() << QString::fromStdString(value);
//        targetMap[id] = value;
//    }
//
//    uint8_t nInstruments = readFromBuffer<uint8_t>(buffer, currentReadLocation);
//    qDebug() << "Instruments: " << nInstruments;
//    std::map<uint16_t, std::string> instrumentMap;
//    for (uint8_t i = 0; i < nInstruments; ++i) {
//        uint16_t id = readFromBuffer<uint16_t>(buffer, currentReadLocation);
//        std::string value = readFromBuffer<std::string>(buffer, currentReadLocation);
//        qDebug() << QString::fromStdString(value);
//        instrumentMap[id] = value;
//    }
//
//    uint32_t nImages = readFromBuffer<uint32_t>(buffer, currentReadLocation);
//    std::vector<Image> images;
//    for (uint32_t i = 0; i < nImages; ++i) {
//        Image image;
//        image.beginning = readFromBuffer<double>(buffer, currentReadLocation);
//        image.ending = readFromBuffer<double>(buffer, currentReadLocation);
//
//        image.beginningString = readFromBuffer<std::string>(buffer, currentReadLocation);
//        image.endingString = readFromBuffer<std::string>(buffer, currentReadLocation);
//        
//        uint8_t targetId = readFromBuffer<uint8_t>(buffer, currentReadLocation);
//        uint16_t instrumentId = readFromBuffer<uint16_t>(buffer, currentReadLocation);
//        image.target = targetMap[targetId];
//        image.instruments = instrumentsFromId(instrumentId, instrumentMap);
//        if (image.instruments.empty())
//            qDebug() << "Instruments were empty";
//        images.push_back(image);
//    }
//
//    _timelineWidget->setData(std::move(images), std::move(targetMap), std::move(instrumentMap));
//
//}
//
//void MainWindow::sendScript(QString script) {
//    if (_socket) {
//        _socket->write(("0" + script + "\r\n").toLatin1());
//        //QByteArray data = (QString("0") + script).toLocal8Bit();
//        //qDebug() << data;
//        //_socket->write(data);
//        //QThread::msleep(25);
//    }
//        //_socket->write(("0" + script + "\r\n").toLatin1());
//        //_socket->write(("0" + script + "\0").toLatin1());
//}
//
//void MainWindow::onSocketConnected() {
//    _socket->write(QString("1\r\n").toLatin1());
//    //_socket->write(QString("1").toLatin1());
//
//}
//
//void MainWindow::onSocketDisconnected() {
//    _configurationWidget->socketDisconnected();
//    _timeControlWidget->socketDisconnected();
//    _informationWidget->socketDisconnected();
//    _timelineWidget->socketDisconnected();
//
//    _informationWidget->logInformation("Disconnected.");
//}
//
//std::string MainWindow::nextTarget() const {
//    return _timelineWidget->nextTarget();
//}
//
//void MainWindow::fullyConnected() {
//    _informationWidget->logInformation("Connected to " + _socket->peerName() + " on port " + QString::number(_socket->peerPort()) + ".");
//
//    _configurationWidget->socketConnected();
//    _timeControlWidget->socketConnected();
//    _informationWidget->socketConnected();
//    _timelineWidget->socketConnected();
//}
