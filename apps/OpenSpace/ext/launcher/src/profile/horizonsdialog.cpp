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
#include <fstream>
#include <iostream>

namespace {
    std::string replaceAll(const std::string& string, const std::string& from, const std::string& to) {
        if (from.empty())
            return "";

        std::string result = string;
        size_t startPos = 0;
        while ((startPos = result.find(from, startPos)) != std::string::npos) {
            result.replace(startPos, from.length(), to);
            startPos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
        }
        return result;
    }

    std::map<int, std::string> parseMatchingBodies(std::filesystem::path& file) {
        std::ifstream fileStream(file);

        if (!fileStream.good()) {
            fileStream.close();
            return std::map<int, std::string>();
        }

        // The beginning of a Horizons file has a header with a lot of information about the
        // query that we do not care about. Ignore everything until head of matching body list
        std::map<int, std::string> matchingBodies;
        std::string line;
        while (fileStream.good() && line.find("Name") == std::string::npos) {
            std::getline(fileStream, line);
        }

        if (!fileStream.good()) {
            fileStream.close();
            return std::map<int, std::string>();
        }

        // There will be one empty line before the list of matching bodies, skip
        std::getline(fileStream, line);
        std::getline(fileStream, line);
        while (fileStream.good()) {
            if (line == " " || line.empty() || line.find("Number of matches") != std::string::npos) {
                fileStream.close();
                return matchingBodies;
            }

            std::cout << "Matching body: " << line << std::endl;
            // Matching body format: ID#, Name, Designation, IAU/aliases/other
            std::stringstream str(line);
            int id;
            std::string name;

            str >> id;
            std::getline(str, name);
            matchingBodies.insert(std::pair<int, std::string>(id, name));

            std::getline(fileStream, line);
        }

        fileStream.close();
        return std::map<int, std::string>();
    }

    std::pair<std::string, std::string> parseValidTimeRange(std::filesystem::path& file) {
        std::ifstream fileStream(file);

        if (!fileStream.good()) {
            fileStream.close();
            return std::pair<std::string, std::string>();
        }

        // The beginning of a Horizons file has a header with a lot of information about the
        // query that we do not care about. Ignore everything until head of time range list
        std::map<int, std::string> matchingBodies;
        std::string line;
        while (fileStream.good() && line.find("Trajectory files") == std::string::npos) {
            std::getline(fileStream, line);
        }

        if (!fileStream.good()) {
            fileStream.close();
            return std::pair<std::string, std::string>();
        }

        // There will be one empty line before the list of time rnages, skip
        std::getline(fileStream, line);
        std::getline(fileStream, line);
        std::string startTime, endTime;

        // From the first line get the start time
        if (fileStream.good()) {
            std::cout << "First time range: " << line << std::endl;
            std::stringstream str(line);
            std::string temp;
            while (str.good()) {
                size_t idx = temp.find('-');
                if (idx != std::string::npos && temp.find('-', idx) != std::string::npos) {
                    startTime = temp; // temp has to be the start date
                    str >> temp;
                    startTime += " T " + temp; // Add the start time
                    break;
                }
                str >> temp;
            }
        }
        if (startTime.empty()) {
            fileStream.close();
            return std::pair<std::string, std::string>();
        }

        // Get the end time from the last trajectery
        while (fileStream.good()) {
            if (line.find("****") != std::string::npos || line.empty() || line == " ") {
                fileStream.close();
                return std::pair<std::string, std::string>(startTime, endTime);
            }

            std::cout << "Time range: " << line << std::endl;
            // Time rnage format: Trajectory file name, start, end

            std::stringstream str(line);
            std::string temp;
            while (str.good()) {
                size_t idx = temp.find('-');
                if (idx != std::string::npos && temp.find('-', idx) != std::string::npos) {
                    // Have found start date, we want end date
                    // start time, end date
                    str >> temp >> temp;
                    endTime = temp;
                    str >> temp;
                    endTime += " T " + temp; // Add the end time
                    break;
                }
                str >> temp;
            }
            std::getline(fileStream, line);
        }

        fileStream.close();
        return std::pair<std::string, std::string>();
    }
} // namespace

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

        _nameEdit = new QLineEdit(QString::fromStdString("horizons.dat"), this);
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

        _chooseTargetCombo = new QComboBox(this);
        _chooseTargetCombo->hide();
        layout->addWidget(_chooseTargetCombo);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* centerLabel = new QLabel("Observer Location:", this);
        container->addWidget(centerLabel);

        _centerEdit = new QLineEdit(QString::fromStdString("500@499"), this);
        container->addWidget(_centerEdit);

        layout->addLayout(container);

        _chooseObserverCombo = new QComboBox(this);
        _chooseObserverCombo->hide();
        layout->addWidget(_chooseObserverCombo);
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

void HorizonsDialog::openHorizonsFile() {
    std::string filePath = QFileDialog::getOpenFileName(
        this,
        tr("Open Horizons file"),
        "",
        tr("Horiozons file (*.dat)")
    ).toStdString();
    _horizonsFile = std::filesystem::absolute(filePath);
    _fileEdit->setText(QString(_horizonsFile.string().c_str()));
}

void HorizonsDialog::openSaveDirectory() {
    std::string directory = QFileDialog::getExistingDirectory(this).toStdString();
    std::cout << "Directory: " << directory << std::endl;
    _directoryEdit->setText(directory.c_str());
}

bool HorizonsDialog::sendHorizonsRequest() {
    // Construct url for request
    std::string url = "";
    url.append(HORIZONS_REQUEST_URL);

    std::string command, targetName;
    if (_chooseTargetCombo->count() > 0 && _chooseTargetCombo->currentIndex() != 0) {
        command = _chooseTargetCombo->itemData(_chooseTargetCombo->currentIndex()).toString().toStdString();
        targetName = _chooseTargetCombo->currentText().toStdString();
    }
    else {
        command = _targetEdit->text().toStdString();
        targetName = command;
    }
    url.append(COMMAND);
    url.append("'");
    url.append(replaceAll(command, " ", SPACE));
    url.append("'");

    std::string center, observerName;
    if (_chooseObserverCombo->count() > 0 && _chooseObserverCombo->currentIndex() != 0) {
        center = "@" +
            _chooseObserverCombo->itemData(_chooseObserverCombo->currentIndex()).toString().toStdString();
        observerName = _chooseObserverCombo->currentText().toStdString();
    }
    else {
        center = _centerEdit->text().toStdString();
        observerName = center;
    }
    url.append(CENTER);
    url.append("'");
    url.append(replaceAll(center, " ", SPACE));
    url.append("'");

    std::string startTime = _startEdit->date().toString("yyyy-MM-dd").toStdString();
    startTime.append(" ");
    startTime.append(_startEdit->time().toString("hh:mm").toStdString());
    url.append(START_TIME);
    url.append("'");
    url.append(replaceAll(startTime, " ", SPACE));
    url.append("'");

    std::string endTime = _endEdit->date().toString("yyyy-MM-dd").toStdString();
    endTime.append(" ");
    endTime.append(_endEdit->time().toString("hh:mm").toStdString());
    url.append(STOP_TIME);
    url.append("'");
    url.append(replaceAll(endTime, " ", SPACE));
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
    // else?

    std::cout << "URL: " << url << std::endl;

    HorizonsDialog::HorizonsResult result = sendRequest(url);
    std::string errorMessage;
    switch (result) {
        case HorizonsDialog::HorizonsResult::Valid:
            std::cout << "Valid result" << std::endl;
            return true;
            break;
        case HorizonsDialog::HorizonsResult::Empty:
            errorMessage = "The generated horizons file is empty";
            break;
        case HorizonsDialog::HorizonsResult::ErrorConnect:
            errorMessage =
                "A connection error occured while downloading the horizons file";
            break;
        case HorizonsDialog::HorizonsResult::ErrorNoObserver:
            errorMessage = "No match was found for observer '" + observerName + "'. "
                "Use '@body' as observer to list possible matches";
            break;
        case HorizonsDialog::HorizonsResult::ErrorMultipleObserver: {
            errorMessage = "Multiple matches was found for observer '" +
                observerName + "'";
            std::map<int, std::string> matchingObservers =
                parseMatchingBodies(_horizonsFile);
            if (matchingObservers.empty()) {
                errorMessage += ". Could not parse the matching observers";
                break;
            }
            _chooseObserverCombo->clear();
            _chooseObserverCombo->addItem("Choose Observer");
            for (std::pair matchingObserver : matchingObservers) {
                _chooseObserverCombo->addItem(
                    matchingObserver.second.c_str(),
                    matchingObserver.first
                );
            }
            _chooseObserverCombo->setCurrentIndex(0);
            _chooseObserverCombo->show();
            break;
        }
        case HorizonsDialog::HorizonsResult::ErrorNoTarget:
            errorMessage = "No match was found for target '" + targetName + "'";
            break;
        case HorizonsDialog::HorizonsResult::ErrorMultipleTarget: {
            errorMessage = "Multiple matches was found for target '" + targetName + "'";
            std::map<int, std::string> matchingTargets =
                parseMatchingBodies(_horizonsFile);
            if (matchingTargets.empty()) {
                errorMessage += ". Could not parse the matching targets";
                break;
            }
            _chooseTargetCombo->clear();
            _chooseTargetCombo->addItem("Choose Target");
            for (std::pair matchingTarget : matchingTargets) {
                _chooseTargetCombo->addItem(
                    matchingTarget.second.c_str(),
                    matchingTarget.first
                );
            }
            _chooseTargetCombo->setCurrentIndex(0);
            _chooseTargetCombo->show();
            break;
        }
        case HorizonsDialog::HorizonsResult::ErrorTimeRange: {
            std::pair<std::string, std::string> validTimeRange =
                parseValidTimeRange(_horizonsFile);
            if (validTimeRange.first.empty()) {
                errorMessage = ". Could not parse the valid time range";
                break;
            }
            errorMessage = "Time range is outside the valid time range for target '"
                + targetName + "'. Valid time range '" + validTimeRange.first + "' to '" +
                validTimeRange.second + "'";
            break;
        }
        case HorizonsDialog::HorizonsResult::ErrorStepSize:
            errorMessage = "Time range '" + startTime + "' to '" + endTime +
                "' with step size '" + _stepEdit->text().toStdString() +
                "' results in a too big file, try to increase the step size or decrease "
                "the time range";
            break;
        case HorizonsDialog::HorizonsResult::UnknownError:
            errorMessage = "An unknown error occured";
            break;
        default:
            errorMessage = "Unknown Result type";
            break;
    }

    _errorMsg->setText(errorMessage.c_str());
    std::filesystem::remove(_horizonsFile);
    return false;
}

// Send request synchronously, EventLoop waits until request has finished
HorizonsDialog::HorizonsResult HorizonsDialog::sendRequest(const std::string url) {
    QNetworkRequest request;
    request.setHeader(QNetworkRequest::UserAgentHeader, "OpenSpace");
    request.setUrl(QUrl(url.c_str()));

    QNetworkReply* reply = _manager->get(request);
    QEventLoop loop;
    auto status = QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    if (!status) {
        std::cout << "Connection failed" << std::endl;
        return HorizonsDialog::HorizonsResult::ErrorConnect;
    }

    loop.exec(QEventLoop::ExcludeUserInputEvents);

    return handleReply(reply);
}

HorizonsDialog::HorizonsResult HorizonsDialog::handleReply(QNetworkReply *reply) {
    if (reply->error()) {
        std::cout << reply->errorString().toStdString();
        reply->deleteLater();
        return HorizonsDialog::HorizonsResult::ErrorConnect;
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
    std::string fullFilePath = filePath.toStdString();
    _horizonsFile = std::filesystem::absolute(fullFilePath);

    reply->deleteLater();
    return isValidHorizonsFile(fullFilePath);
}

HorizonsDialog::HorizonsResult HorizonsDialog::isValidHorizonsFile(const std::string& file) const {
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        return HorizonsDialog::HorizonsResult::Empty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that we can skip. The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    while (line[0] != '$' && fileStream.good()) {
        if (line.find("Revised") != std::string::npos) {
            foundTarget = true;
        }
        else if (line.find("No site matches") != std::string::npos) {
            std::cout << "No matching observer found" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorNoObserver;
        }
        else if (line.find("No ephemeris for target") != std::string::npos) {
            // Valid time range is several lines before this line, harder to parse
            std::cout << "The selected time range is outside the valid range for target"
                << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorTimeRange;
        }
        else if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target or Observer
            if (foundTarget) {
                // If target was found then it is the observer that has multiple matches
                std::cout << "Observer has multiple matches" << std::endl;
                fileStream.close();
                return HorizonsDialog::HorizonsResult::ErrorMultipleObserver;
            }
            else {
                std::cout << "Target has multiple matches" << std::endl;
                fileStream.close();
                return HorizonsDialog::HorizonsResult::ErrorMultipleTarget;
            }
        }
        else if (line.find("No matches found") != std::string::npos) {
            std::cout << "No matches found for target" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorNoTarget;
        }
        else if (line.find("-- change step-size") != std::string::npos) {
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorStepSize;
        }

        std::getline(fileStream, line);
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    fileStream.close();
    if (fileStream.good()) {
        return HorizonsDialog::HorizonsResult::Valid;
    }
    else {
        return HorizonsDialog::HorizonsResult::UnknownError;
    }
}

void HorizonsDialog::approved() {
    // Send request of Horizon file if no local file has been specified
    if (!std::filesystem::is_regular_file(_horizonsFile) && !sendHorizonsRequest()) {
        return;
    }

    std::cout << "File: " << _horizonsFile << std::endl;
    accept();
}
