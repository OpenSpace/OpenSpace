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

#include "shortcutwidget.h"
#include "syncwidget.h"

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/log.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/htmllog.h>

#include <QApplication>
#include <QComboBox>
#include <QDir>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QProcess>
#include <QPushButton>
#include <QThread>

namespace {
    const QSize WindowSize = QSize(640, 480);

    const QString NewsURL = "http://openspace.itn.liu.se/news.txt";

    const std::string _configurationFile = "openspace.cfg";

#ifdef WIN32
    const QString OpenSpaceExecutable = "OpenSpace.exe";
#else
    const QString OpenSpaceExecutable = "OpenSpace";
#endif

    class QLog : public ghoul::logging::Log {
    public:
        void log(
            ghoul::logging::LogManager::LogLevel level,
            const std::string& category,
            const std::string& message
        ) {
            //qDebug() << QString::fromStdString(category) << ": " << QString::fromStdString(message);
        }
    };
}

MainWindow::MainWindow()
    : QWidget(nullptr)
    , _newsReply(nullptr)
    , _informationWidget(nullptr)
    , _scenes(nullptr)
    , _shortcutWidget(nullptr)
    , _syncWidget(nullptr)
{
    setFixedSize(WindowSize);
    
    QGridLayout* layout = new QGridLayout;
    
    QLabel* image = new QLabel;
    QPixmap p = QPixmap(":/images/header.png");
    image->setPixmap(p.scaledToWidth(WindowSize.width()));
    layout->addWidget(image, 0, 0, 1, 2);
    
    _informationWidget = new QTextEdit(this);
    _informationWidget->setReadOnly(true);
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
        _scenes = new QComboBox;
        layout->addWidget(_scenes);
        
        container->setLayout(layout);
    }
    layout->addWidget(container, 1, 1);
    
    container = new QWidget;
    {
        QBoxLayout* layout = new QHBoxLayout;
        
        QPushButton* cancelButton = new QPushButton("Cancel");
        QObject::connect(
            cancelButton, SIGNAL(clicked(bool)),
            QApplication::instance(), SLOT(quit())
        );
        layout->addWidget(cancelButton);
        
        QPushButton* syncButton = new QPushButton("Sync");
        QObject::connect(
            syncButton, SIGNAL(clicked(bool)),
            this, SLOT(syncButtonPressed())
        );
        layout->addWidget(syncButton);
        
        QPushButton* startButton = new QPushButton("Start");
        QObject::connect(
            startButton, SIGNAL(clicked(bool)),
            this, SLOT(startButtonPressed())
        );
        layout->addWidget(startButton);
        
        container->setLayout(layout);
    }
    layout->addWidget(container, 2, 1);
    
    setLayout(layout);
    
    initialize();
}

MainWindow::~MainWindow() {
    delete _informationWidget;
}

void MainWindow::initialize() {
    // Get the news information
    QNetworkRequest request;
    request.setUrl(QUrl(NewsURL));
    
    _newsReply = _networkManager.get(request);
    QObject::connect(_newsReply, SIGNAL(finished()),
            this, SLOT(newsReadyRead())
    );
    QObject::connect(_newsReply, SIGNAL(error(QNetworkReply::NetworkError)),
            this, SLOT(newsNetworkError())
    );

    _shortcutWidget = new ShortcutWidget(this, Qt::Popup | Qt::Dialog);
    _shortcutWidget->setWindowModality(Qt::WindowModal);
    _shortcutWidget->hide();

    _syncWidget = new SyncWidget(this, Qt::Popup | Qt::Dialog);
    _syncWidget->setWindowModality(Qt::WindowModal);
    _syncWidget->hide();

    ghoul::logging::LogManager::initialize(ghoul::logging::LogManager::LogLevel::Debug);
    LogMgr.addLog(new ghoul::logging::ConsoleLog);
    LogMgr.addLog(new ghoul::logging::HTMLLog("LauncherLog.html"));
    LogMgr.addLog(new QLog);

    std::string configurationFile = _configurationFile;
    bool found = openspace::OpenSpaceEngine::findConfiguration(configurationFile);
    if (!found) {
    }

    _configuration = new openspace::ConfigurationManager;
    _configuration->loadFromFile(configurationFile);


    QString modulesDirectory = QString::fromStdString(
        absPath("${SCENE}")
    );
    QDir d(modulesDirectory);
    d.setFilter(QDir::Files);

    QFileInfoList list = d.entryInfoList();
    for (const QFileInfo& i : list) {
        _sceneFiles.insert(i.fileName(), i.absoluteFilePath());
        _scenes->addItem(i.fileName());
    }
    _syncWidget->setSceneFiles(_sceneFiles);
}

void MainWindow::shortcutButtonPressed() {
    _shortcutWidget->show();
}

void MainWindow::syncButtonPressed() {
    _syncWidget->show();
}

void MainWindow::startButtonPressed() {
    QProcess::startDetached(OpenSpaceExecutable);
}

void MainWindow::newsNetworkError() {
    QString error = _newsReply->errorString();
    _informationWidget->setText(error);
    _newsReply->deleteLater();
}

void MainWindow::newsReadyRead() {
    QByteArray data = _newsReply->readAll();
    QString news = QString::fromLatin1(data);
    _informationWidget->setText(news);
    _newsReply->deleteLater();
}
