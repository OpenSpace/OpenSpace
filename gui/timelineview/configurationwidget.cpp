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

#include "configurationwidget.h"

#include <QGridLayout>
#include <QLabel>
#include <QTimer>
#include <QDebug>

ConfigurationWidget::ConfigurationWidget(QWidget* parent)
    : QGroupBox("Connection", parent)
    , _ipAddress(new QLineEdit("localhost"))
    , _port(new QLineEdit("20500"))
    , _connect(new QPushButton("Connect"))
{
    QGridLayout* layout = new QGridLayout;
    layout->setVerticalSpacing(0);
    {
        QLabel* t = new QLabel("IP Address");
        layout->addWidget(t, 0, 0);
    }
    layout->addWidget(_ipAddress, 1, 0);

    {
        QLabel* t = new QLabel("Port");
        layout->addWidget(t, 0, 1);
    }
    layout->addWidget(_port, 1, 1);
    layout->addWidget(_connect, 1, 2, 1, 1);

    setLayout(layout);

    QObject::connect(_connect, SIGNAL(clicked()), this, SLOT(onConnectButton()));

    QTimer::singleShot(100, this, SLOT(onConnectButton()));
}

void ConfigurationWidget::onConnectButton() {
    emit connect(_ipAddress->text(), _port->text());
}

void ConfigurationWidget::socketConnected() {
    _ipAddress->setEnabled(false);
    _port->setEnabled(false);
    _connect->setText("Disconnect");
    QObject::disconnect(_connect, SIGNAL(clicked()), this, SLOT(onConnectButton()));
    QObject::connect(_connect, SIGNAL(clicked()), this, SIGNAL(disconnect()));
}

void ConfigurationWidget::socketDisconnected() {
    _ipAddress->setEnabled(true);
    _port->setEnabled(true);
    _connect->setText("Connect");
    QObject::disconnect(_connect, SIGNAL(clicked()), this, SIGNAL(disconnect()));
    QObject::connect(_connect, SIGNAL(clicked()), this, SLOT(onConnectButton()));
}
