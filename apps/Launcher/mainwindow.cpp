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

#include "mainwindow.h"

#include "shortcutwidget.h"
#include "syncwidget.h"

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/logfactory.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/log.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/htmllog.h>
#include <ghoul/logging/visualstudiooutputlog.h>

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

    const QString NewsURL = "http://openspaceproject.com/news.txt";

    const std::string _configurationFile = "openspace.cfg";

#ifdef WIN32
    const QString OpenSpaceExecutable = "OpenSpace.exe";
#else
    const QString OpenSpaceExecutable = "./OpenSpace";
#endif

    class QLog : public ghoul::logging::Log {
    public:
        void log(
            ghoul::logging::LogLevel level,
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
    setObjectName("MainWindow");
    setFixedSize(WindowSize);
    //setContentsMargins(0, 0, 0, 0);
    
    QGridLayout* layout = new QGridLayout;
    layout->setContentsMargins(0, 0, 0, 0);
    
    QLabel* image = new QLabel;
    //image->setContentsMargins(0, 0, 0, 0);
    image->setObjectName("Image");
    QPixmap p = QPixmap(":/images/header.png");
    image->setPixmap(p.scaledToWidth(WindowSize.width()));
    layout->addWidget(image, 0, 0, 1, 2);

    
    _informationWidget = new QTextEdit(this);
    _informationWidget->setReadOnly(true);
    //_informationWidget->setEnabled(false);
    layout->addWidget(_informationWidget, 1, 0, 2, 1);
    layout->setRowStretch(1, 10);
    layout->setColumnStretch(0, 4);
    layout->setColumnStretch(1, 5);

    QWidget* container = new QWidget;
    {
        QGridLayout* innerLayout = new QGridLayout;
        
        //QLabel* shortcutLabel = new QLabel("Keyboard Shortcuts:");
        //innerLayout->addWidget(shortcutLabel, 0, 0);
        //QPushButton* shortcutButton = new QPushButton("Open...");
        //QObject::connect(shortcutButton, SIGNAL(clicked(bool)),
        //                 this, SLOT(shortcutButtonPressed())
        //                 );
        //innerLayout->addWidget(shortcutButton, 0, 1);

        innerLayout->setRowStretch(1, 10);

        QLabel* configurationSelectionLabel = new QLabel("Configuration:");
        innerLayout->addWidget(configurationSelectionLabel, 2, 0);
        _configurations = new QComboBox;
        innerLayout->addWidget(_configurations, 2, 1);
        
        QLabel* sceneSelectionLabel = new QLabel("Scenes:");
        innerLayout->addWidget(sceneSelectionLabel, 3, 0);
        _scenes = new QComboBox;
        innerLayout->addWidget(_scenes, 3, 1);
        
        container->setLayout(innerLayout);
    }
    layout->addWidget(container, 1, 1);
    
    container = new QWidget;
    {
        QBoxLayout* innerLayout = new QHBoxLayout;
        
        QPushButton* cancelButton = new QPushButton("Cancel");
        QObject::connect(
            cancelButton, SIGNAL(clicked(bool)),
            QApplication::instance(), SLOT(quit())
        );
        innerLayout->addWidget(cancelButton);
        
        QPushButton* syncButton = new QPushButton("Sync");
        QObject::connect(
            syncButton, SIGNAL(clicked(bool)),
            this, SLOT(syncButtonPressed())
        );
        innerLayout->addWidget(syncButton);
        
        QPushButton* startButton = new QPushButton("Start");
        QObject::connect(
            startButton, SIGNAL(clicked(bool)),
            this, SLOT(startButtonPressed())
        );
        innerLayout->addWidget(startButton);
        
        container->setLayout(innerLayout);
    }
    layout->addWidget(container, 2, 1);
    
    setLayout(layout);
    
    initialize();
}

MainWindow::~MainWindow() {
    delete _informationWidget;
}


void MainWindow::configureLogging() {
    const std::string KeyLogLevel =
    openspace::ConfigurationManager::KeyLauncher + '.' + openspace::ConfigurationManager::PartLogLevel;
    const std::string KeyLogImmediateFlush =
    openspace::ConfigurationManager::KeyLauncher + '.' + openspace::ConfigurationManager::PartImmediateFlush;
    const std::string KeyLogs =
    openspace::ConfigurationManager::KeyLauncher + '.' + openspace::ConfigurationManager::PartLogs;

    if (_configuration->hasKeyAndValue<std::string>(KeyLogLevel)) {
        std::string logLevel;
        _configuration->getValue(KeyLogLevel, logLevel);

        bool immediateFlush = false;
        _configuration->getValue(KeyLogImmediateFlush, immediateFlush);

        ghoul::logging::LogLevel level = ghoul::logging::levelFromString(logLevel);
        LogMgr.deinitialize();
        using ImmediateFlush = ghoul::logging::LogManager::ImmediateFlush;
        LogMgr.initialize(
                               level,
                               immediateFlush ? ImmediateFlush::Yes : ImmediateFlush::No
                               );
        LogMgr.addLog(std::make_unique<ghoul::logging::ConsoleLog>());
        // TODO: This can crash the system in cases where the logfile can't be created ---abock
        LogMgr.addLog(std::make_unique< ghoul::logging::HTMLLog >("LauncherLog.html", ghoul::logging::HTMLLog::Append::No));
        LogMgr.addLog(std::make_unique< QLog >());
    }

    if (_configuration->hasKeyAndValue<ghoul::Dictionary>(KeyLogs)) {
        ghoul::Dictionary logs;
        _configuration->getValue(KeyLogs, logs);

        for (size_t i = 1; i <= logs.size(); ++i) {
            ghoul::Dictionary logInfo;
            logs.getValue(std::to_string(i), logInfo);

            try {
                LogMgr.addLog(openspace::createLog(logInfo));
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

#ifdef WIN32
    if (IsDebuggerPresent()) {
        LogMgr.addLog(std::make_unique<ghoul::logging::VisualStudioOutputLog>());
    }
#endif // WIN32

#ifndef GHOUL_LOGGING_ENABLE_TRACE
    std::string logLevel;
    _configuration->getValue(KeyLogLevel, logLevel);
    LogLevel level = ghoul::logging::levelFromString(logLevel);

    if (level == ghoul::logging::LogLevel::Trace) {
        LWARNING("Desired logging level is set to 'Trace' but application was " <<
                 "compiled without Trace support");
    }
#endif // GHOUL_LOGGING_ENABLE_TRACE

//    printf("%d", _optionParser->value("d").toInt());
//    ghoul::logging::LogManager::initialize(static_cast<ghoul::logging::LogLevel>(_optionParser->value("d").toInt()));
//    LogMgr.addLog( std::make_unique< ghoul::logging::ConsoleLog >() );


}

void MainWindow::initialize() {
    // ParseOptions
    generateOptions();

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

    std::string configurationFile = _configurationFile;

    _configuration = new openspace::ConfigurationManager;
    configurationFile = _configuration->findConfiguration( configurationFile );
    _configuration->loadFromFile(configurationFile);

    configureLogging();

    // Load all available scenes
    QString modulesDirectory = QString::fromStdString(absPath("${SCENE}"));
    QDir d(modulesDirectory);
    d.setFilter(QDir::Files);
    QFileInfoList list = d.entryInfoList();
    _scenes->addItem("Use Default");
    for (const QFileInfo& i : list) {
        QString file = i.fileName();
        file = file.replace(".scene", "");
        _sceneFiles.insert(file, i.absoluteFilePath());
        _scenes->addItem(file);
    }
    _scenes->setCurrentText("Use Default");
    _syncWidget->setSceneFiles(_sceneFiles);

    // Load all available configuration files
    QString configurationDirectory = QString::fromStdString(absPath("${SGCT}"));
    d = QDir(configurationDirectory);
    d.setFilter(QDir::Files);
    list = d.entryInfoList();
    _configurations->addItem("Use Default");
    for (const QFileInfo& i : list) {
        QString file = i.fileName();
        file = file.replace(".xml", "");
        _configurationFiles.insert(file, i.absoluteFilePath());
        _configurations->addItem(file);
    }
    _configurations->setCurrentText("Use Default");
}

void MainWindow::shortcutButtonPressed() {
    _shortcutWidget->show();
}

void MainWindow::syncButtonPressed() {
    _syncWidget->show();
}

void MainWindow::startButtonPressed() {
    QString exec = OpenSpaceExecutable;
    if (_sceneFiles.contains(_scenes->currentText()))
        exec += " -scene \"" + _sceneFiles[_scenes->currentText()] + "\"";

    if (_configurationFiles.contains(_configurations->currentText()))
        exec += " -sgct \"" + _configurationFiles[_configurations->currentText()] + "\"";

    LINFOC("MainWindow", "Executing: " << exec.toStdString());
    QProcess::startDetached(exec);
}

void MainWindow::newsNetworkError() {
    QString error = _newsReply->errorString();
    _informationWidget->setText(error);
    _newsReply->deleteLater();
}

void MainWindow::newsReadyRead() {
    QByteArray arrayData = _newsReply->readAll();
    QString news = QString::fromLatin1(arrayData);
    _informationWidget->setText(news);
    _newsReply->deleteLater();
}

void MainWindow::generateOptions() {

    _optionParser = new QCommandLineParser;
    _optionParser->setApplicationDescription("OpenSpace Launcher");
    _optionParser->addHelpOption();
    _optionParser->addVersionOption();

    _optionParser->addOptions({
        { { "d", "debug" },
        QCoreApplication::translate("main", "Debug output level"),
        QCoreApplication::translate("main", "0, 1, 2, 3, 4, 5, 6"),
        QCoreApplication::translate("main", "1"),
        }
    });
    _optionParser->process(*QApplication::instance());
}
