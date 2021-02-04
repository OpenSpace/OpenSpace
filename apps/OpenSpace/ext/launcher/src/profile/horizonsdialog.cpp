/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

 // Things needed to construct the url for the http request to JPL Horizons interface
#define HORIZONS_REQUEST_URL "https://ssd.jpl.nasa.gov/horizons_batch.cgi?batch=1&MAKE_EPHEM='YES'&TABLE_TYPE='OBSERVER'&QUANTITIES='20,33'&RANGE_UNITS='KM'&SUPPRESS_RANGE_RATE='YES'&CSV_FORMAT='NO'"
#define COMMAND "&COMMAND="
#define CENTER "&CENTER="
#define START_TIME "&START_TIME="
#define STOP_TIME "&STOP_TIME="
#define STEP_SIZE "&STEP_SIZE="
#define SPACE "%20"
#define TIMEVARYING "arcseconds (time-varying)"
#define MINUTES "minutes"
#define HOURS "hours"
#define DAYS "days"
#define MONTHS "calendar months"
#define YEARS "calendar years"
#define UNITLESS "equal intervals (unitless)"

#include "profile/horizonsdialog.h"

#include "profile/line.h"
#include <QComboBox>
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QDir>
#include <QEventLoop>
#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QPushButton>
#include <QVBoxLayout>
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>

HorizonsDialog::HorizonsDialog(QWidget* parent)
    : QDialog(parent)
{
    _manager = new QNetworkAccessManager(this);

    setWindowTitle("Horizons");
    createWidgets();

    QStringList timeTypes = {
        TIMEVARYING,
        MINUTES,
        HOURS,
        DAYS,
        MONTHS,
        YEARS,
        UNITLESS
    };
    _timeTypeCombo->addItems(timeTypes);
    _timeTypeCombo->setCurrentIndex(1);
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* localLabel = new QLabel("Select a local Horizons file:", this);
        localLabel->setObjectName("heading");
        layout->addWidget(localLabel);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* fileLabel = new QLabel("File:", this);
        container->addWidget(fileLabel);

        _fileEdit = new QLineEdit(this);
        container->addWidget(_fileEdit);

        QPushButton* fileButton = new QPushButton("Browse", this);
        connect(
            fileButton,
            &QPushButton::released,
            this,
            &HorizonsDialog::openHorizonsFile
        );
        fileButton->setCursor(Qt::PointingHandCursor);
        container->addWidget(fileButton);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        QLabel* generateLabel = new QLabel("Or generate a new Horizons file:", this);
        generateLabel->setObjectName("heading");
        layout->addWidget(generateLabel);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* nameLabel = new QLabel("Filename:", this);
        container->addWidget(nameLabel);

        _nameEdit = new QLineEdit(QString::fromStdString("Horizons.dat"), this);
        _nameEdit->setToolTip("Name of the generated Horizons file. Must end with '.dat'");
        container->addWidget(_nameEdit);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* nameLabel = new QLabel("Save directory:", this);
        container->addWidget(nameLabel);

        _directoryEdit = new QLineEdit(this);
        _directoryEdit->setToolTip("Directory where the generated Horizons file is saved");
        container->addWidget(_directoryEdit);

        QPushButton* directoryButton = new QPushButton("Browse", this);
        connect(
            directoryButton,
            &QPushButton::released,
            this,
            &HorizonsDialog::openSaveDirectory
        );
        directoryButton->setCursor(Qt::PointingHandCursor);
        container->addWidget(directoryButton);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* targetLabel = new QLabel("Target Body:", this);
        container->addWidget(targetLabel);

        _targetEdit = new QLineEdit(QString::fromStdString("Mars Reconnaissance Orbiter"), this);
        container->addWidget(_targetEdit);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* centerLabel = new QLabel("Observer Location:", this);
        container->addWidget(centerLabel);

        _centerEdit = new QLineEdit(QString::fromStdString("500@4"), this);
        container->addWidget(_centerEdit);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* startLabel = new QLabel("Start Time:", this);
        container->addWidget(startLabel);
        _startEdit = new QDateTimeEdit(this);
        _startEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm");
        _startEdit->setDate(QDate::currentDate().addDays(-1));
        container->addWidget(_startEdit);
        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* endLabel = new QLabel("End Time:", this);
        container->addWidget(endLabel);
        _endEdit = new QDateTimeEdit(this);
        _endEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm");
        _endEdit->setDate(QDate::currentDate());
        container->addWidget(_endEdit);
        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* stepLabel = new QLabel("Step Size:", this);
        container->addWidget(stepLabel);

        _stepEdit = new QLineEdit(this);
        _stepEdit->setValidator(new QIntValidator(this));
        _stepEdit->setText(QString::number(10));
        container->addWidget(_stepEdit);

        _timeTypeCombo = new QComboBox(this);
        container->addWidget(_timeTypeCombo);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* footer = new QHBoxLayout(this);
        _errorMsg = new QLabel(this);
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox(this);
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &HorizonsDialog::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &HorizonsDialog::reject);
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }
}

// Send request synchronously, EventLoop waits until request has finished
void HorizonsDialog::sendRequest(const std::string url) {
    QNetworkRequest request;
    request.setHeader(QNetworkRequest::UserAgentHeader, "OpenSpace");
    request.setUrl(QUrl(url.c_str()));

    QNetworkReply *reply = _manager->get(request);
    QEventLoop loop;
    auto status = QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    if (!status) {
        std::cout << "Connection failed" << std::endl;
        return;
    }

    loop.exec(QEventLoop::ExcludeUserInputEvents);

    handleReply(reply);
}

void HorizonsDialog::openHorizonsFile() {
    _horizonsFile = QFileDialog::getOpenFileName(
        this,
        tr("Open Horizons file"),
        "",
        tr("Horiozons file (*.dat)")
    ).toStdString();
    _fileEdit->setText(_horizonsFile.c_str());
}

void HorizonsDialog::openSaveDirectory() {
    std::string directory = QFileDialog::getExistingDirectory(this).toStdString();
    std::cout << "Directory: " << directory << std::endl;
    _directoryEdit->setText(directory.c_str());
}

void replaceAll(std::string& string, const std::string& from, const std::string& to) {
    if (from.empty())
        return;
    size_t startPos = 0;
    while ((startPos = string.find(from, startPos)) != std::string::npos) {
        string.replace(startPos, from.length(), to);
        startPos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

void HorizonsDialog::sendHorizonsRequest() {
    // Construct url for request
    std::string url = "";
    url.append(HORIZONS_REQUEST_URL);

    std::string command = _targetEdit->text().toStdString();
    replaceAll(command, " ", SPACE);
    url.append(COMMAND);
    url.append("'");
    url.append(command);
    url.append("'");;

    std::string center = _centerEdit->text().toStdString();
    replaceAll(center, " ", SPACE);
    url.append(CENTER);
    url.append("'");
    url.append(center);
    url.append("'");

    url.append(START_TIME);
    url.append("'");
    url.append(_startEdit->date().toString("yyyy-MM-dd").toStdString());
    url.append(SPACE);
    url.append(_startEdit->time().toString("hh:mm").toStdString());
    url.append("'");

    url.append(STOP_TIME);
    url.append("'");
    url.append(_endEdit->date().toString("yyyy-MM-dd").toStdString());
    url.append(SPACE);
    url.append(_endEdit->time().toString("hh:mm").toStdString());
    url.append("'");

    url.append(STEP_SIZE);
    url.append("'");
    url.append(_stepEdit->text().toStdString());
    url.append(SPACE);
    if (_timeTypeCombo->currentText().toStdString() == TIMEVARYING) {
        url.append("VAR'");
    }
    else if (_timeTypeCombo->currentText().toStdString() == MINUTES) {
        url.append("m'");
    }
    else if (_timeTypeCombo->currentText().toStdString() == HOURS) {
        url.append("h'");
    }
    else if (_timeTypeCombo->currentText().toStdString() == DAYS) {
        url.append("d'");
    }
    else if (_timeTypeCombo->currentText().toStdString() == MONTHS) {
        url.append("MO'");
    }
    else if (_timeTypeCombo->currentText().toStdString() == YEARS) {
        url.append("Y'");
    }

    std::cout << "URL: " << url << std::endl;

    sendRequest(url);
}

void HorizonsDialog::handleReply(QNetworkReply *reply) {
    if (reply->error()) {
        std::cout << reply->errorString().toStdString();
        return;
    }

    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (redirect.isValid() && reply->url() != redirect) {
        std::cout << "Redirect has been requested" << std::endl;
    }

    QString answer = reply->readAll();

    std::cout << "Reply: '";
    std::cout << answer.toStdString();
    std::cout << "'" << std::endl;

    // Create a text file and write reply to it
    QString filePath = _directoryEdit->text();
    filePath.append(QDir::separator());
    filePath.append(_nameEdit->text());

    std::ofstream file(filePath.toStdString());
    file << answer.toStdString() << std::endl;

    file.close();
    _horizonsFile = std::filesystem::absolute(filePath.toStdString()).string();

    reply->deleteLater();
}

void HorizonsDialog::approved() {
    if (_horizonsFile.empty()) {
        // Send request of Horizon file if no local file has been specified
        sendHorizonsRequest();

        if (_horizonsFile.empty()) {
            _errorMsg->setText(
                "At least one Horizons file must be specified or generated"
            );
            return;
        }
    }

    std::cout << "File: " << _horizonsFile << std::endl;
    accept();
}
