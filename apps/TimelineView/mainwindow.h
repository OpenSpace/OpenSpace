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

#ifndef __MAINWINDOW_H__
#define __MAINWINDOW_H__

#include <QWidget>
#include <QTcpSocket>

#include "common.h"

class ConfigurationWidget;
class ControlWidget;
class InformationWidget;
class TimelineWidget;

class MainWindow : public QWidget {
Q_OBJECT
public:
	MainWindow();
	~MainWindow();

    std::string nextTarget() const;

public slots:
    void sendScript(QString script);

private slots:
    void onConnect(QString host, QString port);
    void onDisconnect();

    void onSocketConnected();
    void onSocketDisconnected();

	void readTcpData();
    void handleStatusMessage(QByteArray data);
    void handlePlaybook(QByteArray data);

    void fullyConnected();

private:
    ConfigurationWidget* _configurationWidget;
    ControlWidget* _timeControlWidget;
    InformationWidget* _informationWidget;
    TimelineWidget* _timelineWidget;
    
	QTcpSocket* _socket;

    bool _isConnected = false;
};

#endif // __MAINWINDOW_H__
