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

#ifndef __OPENSPACE_APP_LAUNCHER___MAINWINDOW___H__
#define __OPENSPACE_APP_LAUNCHER___MAINWINDOW___H__

#include <QWidget>

#include <QMap>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QTextEdit>

class QComboBox;
class QNetworkAccessManager;

class ShortcutWidget;
class SyncWidget;

namespace openspace {
    class ConfigurationManager;
}

class MainWindow : public QWidget {
Q_OBJECT
public:
    MainWindow();
    ~MainWindow();

private slots:
    void shortcutButtonPressed();
    void syncButtonPressed();
    void startButtonPressed();
    
    void newsNetworkError();
    void newsReadyRead();

private:
    void initialize();
    
    QNetworkReply* _newsReply;
    
    QTextEdit* _informationWidget;

    QComboBox* _configurations;
    QMap<QString, QString> _configurationFiles;

    QComboBox* _scenes;
    QMap<QString, QString> _sceneFiles;
    
    ShortcutWidget* _shortcutWidget;
    SyncWidget* _syncWidget;

    QNetworkAccessManager _networkManager;
    openspace::ConfigurationManager* _configuration;
};

//class MainWindow : public QWidget {
//Q_OBJECT
//public:
//    MainWindow();
//    ~MainWindow();
//
//    std::string nextTarget() const;
//
//public slots:
//    void sendScript(QString script);
//
//private slots:
//    void onConnect(QString host, QString port);
//    void onDisconnect();
//
//    void onSocketConnected();
//    void onSocketDisconnected();
//
//    void readTcpData();
//    void handleStatusMessage(QByteArray data);
//    void handlePlaybook(QByteArray data);
//
//    void fullyConnected();
//
//private:
//    ConfigurationWidget* _configurationWidget;
//    ControlWidget* _timeControlWidget;
//    InformationWidget* _informationWidget;
//    TimelineWidget* _timelineWidget;
//    
//    QTcpSocket* _socket;
//
//    bool _hasHongKangTimeline = false;
//    bool _hasLabelTimeline = false;
//};

#endif // __OPENSPACE_APP_LAUNCHER___MAINWINDOW___H__
