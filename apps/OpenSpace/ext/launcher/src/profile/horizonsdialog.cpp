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

#include "profile/horizonsdialog.h"

#include "profile/line.h"
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QEventLoop>
#include <QFileDialog>
#include <QHeaderView>
#include <QLabel>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QPushButton>
#include <QSslConfiguration>
#include <QVBoxLayout>
#include <iostream>

HorizonsDialog::HorizonsDialog(QWidget* parent)
    : QDialog(parent), _config(QSslConfiguration::defaultConfiguration())
{
    _manager = new QNetworkAccessManager();
    _config.setProtocol(QSsl::TlsV1_2);

    setWindowTitle("Horizons");
    createWidgets();
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        QLabel* localLabel = new QLabel("Select a local Horizons file");
        localLabel->setObjectName("heading");
        container->addWidget(localLabel, 0, 0);

        QPushButton* horizonsFileButton = new QPushButton("Browse", this);
        connect(
            horizonsFileButton, &QPushButton::released,
            [this]() {
                openHorizonsFile();
            }
        );
        horizonsFileButton->setCursor(Qt::PointingHandCursor);
        container->addWidget(horizonsFileButton, 0, 2);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* container = new QHBoxLayout;
        QLabel* generateLabel = new QLabel("Or generate a new Horizons file");
        generateLabel->setObjectName("heading");
        container->addWidget(generateLabel);

        layout->addLayout(container);
    }
    {
        _startLabel = new QLabel("Start Time:");
        layout->addWidget(_startLabel);
        _startEdit = new QDateTimeEdit;
        _startEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm");
        _startEdit->setDate(QDate::currentDate().addDays(-1));
        layout->addWidget(_startEdit);
    }
    {
        _endLabel = new QLabel("End Time:");
        layout->addWidget(_endLabel);
        _endEdit = new QDateTimeEdit;
        _endEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm");
        _endEdit->setDate(QDate::currentDate());
        layout->addWidget(_endEdit);
    }
    {
        QBoxLayout* footer = new QHBoxLayout;
        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &HorizonsDialog::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &HorizonsDialog::reject);
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }
}

// Send HTTPS request synchronously, EventLoop waits until request has finished
void HorizonsDialog::sendRequest(const std::string url) {
    QNetworkRequest request;
    request.setSslConfiguration(_config);
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
}

void HorizonsDialog::sendHorizonsRequest() {
    // Construct url for https request
    std::string url = "";
    url.append(HORIZONS_REQUEST_URL);
    url.append(COMMAND); url.append("'-74'");
    url.append(CENTER); url.append("'500@4'");

    url.append(START_TIME); url.append("'");
    url.append(_startEdit->date().toString("yyyy-MM-dd").toStdString());
    url.append(SPACE);
    url.append(_startEdit->time().toString("hh:mm").toStdString());
    url.append("'");

    url.append(STOP_TIME); url.append("'");
    url.append(_endEdit->date().toString("yyyy-MM-dd").toStdString());
    url.append(SPACE);
    url.append(_endEdit->time().toString("hh:mm").toStdString());
    url.append("'");

    url.append(STEP_SIZE); url.append("'10"); url.append(SPACE); url.append("m'");

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

    reply->deleteLater();
}

void HorizonsDialog::approved() {
    if (_horizonsFile.empty()) {
        // Send request of Horizon file if no local file has been specified
        sendHorizonsRequest();
    }

    accept();
}

HorizonsDialog::~HorizonsDialog() {
    _manager->deleteLater();
}
