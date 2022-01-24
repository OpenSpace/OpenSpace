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
#define HORIZONS_REQUEST_URL "https://ssd.jpl.nasa.gov/api/horizons.api?format=json&MAKE_EPHEM='YES'&TABLE_TYPE='OBSERVER'&QUANTITIES='20,33'&RANGE_UNITS='KM'&SUPPRESS_RANGE_RATE='YES'&CSV_FORMAT='NO'"
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
#include <sstream>

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

    std::string trim(const std::string& text) {
        std::string result = text;
        if (text.empty())
            return "";

        // front
        while (result.front() == ' ') {
            result = result.substr(1);
        }

        // end
        while (result.back() == ' ') {
            result = result.substr(0, result.length() - 1);
        }

        // middle
        int offset = 0;
        int start = -1;
        int step = 1;
        // Find start of gap
        while ((start = result.find_first_of(' ', offset)) != std::string::npos) {
            // Widen gap
            while (result[start + step] == ' ') {
                ++step;
            }

            // Measure gap, if big then erase it
            if (step > 1) {
                result = result.erase(start, step);

                // Add one whitespace for readability
                result = result.insert(start, " ");

                step = 1;
                offset = start;
            }

            // Keep looking
            offset = start + step;
            step = 1;
        }

        return result;
    }

    std::map<int, std::string> parseBodies(std::filesystem::path& file) {
        std::ifstream fileStream(file);
        std::string startPhrase = "Name";
        std::string endPhrase = "matches";

        if (!fileStream.good()) {
            fileStream.close();
            return std::map<int, std::string>();
        }

        // The beginning of a Horizons file has a header with a lot of information about the
        // query that we do not care about. Ignore everything until head of matching body list
        std::map<int, std::string> matchingBodies;
        std::string line;
        while (fileStream.good() && line.find(startPhrase) == std::string::npos) {
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
            if (line == " " || line.empty() || line.find(endPhrase) != std::string::npos) {
                fileStream.close();
                return matchingBodies;
            }

            std::cout << "Matching body: " << line << std::endl;
            // Matching body format: id, other information...
            std::stringstream str(line);
            int id;
            std::string info;

            str >> id;
            std::getline(str, info);
            matchingBodies.insert(std::pair<int, std::string>(id, trim(info)));

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

std::filesystem::path HorizonsDialog::file() const {
    return _horizonsFile;
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* generateLabel = new QLabel("Generate a new Horizons file:", this);
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
        QLabel* directoryLabel = new QLabel("Save directory:", this);
        container->addWidget(directoryLabel);

        _directoryEdit = new QLineEdit(this);
        _directoryEdit->setToolTip("Directory where the generated Horizons file is saved");
        container->addWidget(_directoryEdit);

        QPushButton* directoryButton = new QPushButton("Browse", this);
        connect(
            directoryButton,
            &QPushButton::released,
            this,
            &HorizonsDialog::openDirectory
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
        _targetEdit->setToolTip("Which target or body would you like Horizons trajectery data for?");
        container->addWidget(_targetEdit);

        layout->addLayout(container);

        _chooseTargetCombo = new QComboBox(this);
        _chooseTargetCombo->hide();
        _chooseTargetCombo->setToolTip("Choose a target from the search, or search again");
        layout->addWidget(_chooseTargetCombo);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* centerLabel = new QLabel("Observer Location:", this);
        container->addWidget(centerLabel);

        _centerEdit = new QLineEdit(QString::fromStdString("500@499"), this);
        _centerEdit->setToolTip("In which reference frame do you want the data in?");
        container->addWidget(_centerEdit);

        layout->addLayout(container);

        _chooseObserverCombo = new QComboBox(this);
        _chooseObserverCombo->hide();
        _chooseObserverCombo->setToolTip("Choose an observer from the search, or search again");
        layout->addWidget(_chooseObserverCombo);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* startLabel = new QLabel("Start Time:", this);
        container->addWidget(startLabel);
        _startEdit = new QDateTimeEdit(this);
        _startEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm");
        _startEdit->setDate(QDate::currentDate().addDays(-1));
        _startEdit->setToolTip("Enter the start date and time for the data");
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
        _endEdit->setToolTip("Enter the end date and time for the data");
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
        _stepEdit->setToolTip("Enter the step size for the data");
        container->addWidget(_stepEdit);

        _timeTypeCombo = new QComboBox(this);
        _timeTypeCombo->setToolTip("Choose unit of the step size");
        container->addWidget(_timeTypeCombo);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        _downloadLabel = new QLabel("Downloading file...", this);
        _downloadLabel->hide();
        layout->addWidget(_downloadLabel, 0, Qt::AlignRight);
    }
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

void HorizonsDialog::openFile() {
    std::string filePath = QFileDialog::getOpenFileName(
        this,
        tr("Open Horizons file"),
        "",
        tr("Horiozons file (*.dat)")
    ).toStdString();
    _horizonsFile = std::filesystem::absolute(filePath);
    //_fileEdit->setText(QString(_horizonsFile.string().c_str()));
}

void HorizonsDialog::openDirectory() {
    std::string directory = QFileDialog::getExistingDirectory(this).toStdString();
    _directoryEdit->setText(directory.c_str());
}

bool HorizonsDialog::handleRequest() {
    if (!isValidInput()) {
        return false;
    }

    std::string url = constructUrl();

    _chooseObserverCombo->clear();
    _chooseObserverCombo->hide();
    _chooseTargetCombo->clear();
    _chooseTargetCombo->hide();

    HorizonsDialog::HorizonsResult result = sendRequest(url);
    return handleResult(result);
}

bool HorizonsDialog::isValidInput() {
    bool isValid = true;
    std::string message;

    // Request
    // Name
    if (_nameEdit->text().isEmpty()) {
        isValid = false;
        message = "Filename not selected";
    }

    // Directory
    else if (_directoryEdit->text().isEmpty()) {
        isValid = false;
        message = "Directory not selected";
    }
    else if (!std::filesystem::is_directory(_directoryEdit->text().toStdString())) {
        isValid = false;
        message = "The selected directory could not be found";
    }

    // Target field
    else if (_targetEdit->text().isEmpty() &&
            (_chooseTargetCombo->count() > 0 &&
             _chooseTargetCombo->currentIndex() == 0))
    {
        isValid = false;
        message = "Target not selected";
    }
    else if (_targetEdit->text().isEmpty() && _chooseTargetCombo->count() == 0) {
        isValid = false;
        message = "Target not selected";
    }

    // Observer field
    else if (_centerEdit->text().isEmpty() &&
            (_chooseObserverCombo->count() > 0 &&
             _chooseObserverCombo->currentIndex() == 0))
    {
        isValid = false;
        message = "Observer not selected";
    }
    else if (_centerEdit->text().isEmpty() && _chooseObserverCombo->count() == 0) {
        isValid = false;
        message = "Observer not selected";
    }

    // Step size
    else if (_stepEdit->text().isEmpty()) {
        isValid = false;
        message = "Step size not selected";
    }

    _errorMsg->setText(message.c_str());
    return isValid;
}

std::string HorizonsDialog::constructUrl() {
    // Construct url for request
    std::string url = "";
    url.append(HORIZONS_REQUEST_URL);

    std::string command;
    if (_chooseTargetCombo->count() > 0 && _chooseTargetCombo->currentIndex() != 0) {
        command = _chooseTargetCombo->itemData(_chooseTargetCombo->currentIndex()).toString().toStdString();
        _targetName = _chooseTargetCombo->currentText().toStdString();
        _targetEdit->setText(_targetName.c_str());
    }
    else {
        command = _targetEdit->text().toStdString();
        _targetName = command;
    }
    url.append(COMMAND);
    url.append("'");
    url.append(replaceAll(command, " ", SPACE));
    url.append("'");

    std::string center;
    if (_chooseObserverCombo->count() > 0 && _chooseObserverCombo->currentIndex() != 0) {
        center = "@" +
            _chooseObserverCombo->itemData(_chooseObserverCombo->currentIndex()).toString().toStdString();
        _observerName = _chooseObserverCombo->currentText().toStdString();
        _centerEdit->setText(_observerName.c_str());
    }
    else {
        center = _centerEdit->text().toStdString();
        _observerName = center;
    }
    url.append(CENTER);
    url.append("'");
    url.append(replaceAll(center, " ", SPACE));
    url.append("'");

    _startTime = _startEdit->date().toString("yyyy-MM-dd").toStdString();
    _startTime.append(" ");
    _startTime.append(_startEdit->time().toString("hh:mm").toStdString());
    url.append(START_TIME);
    url.append("'");
    url.append(replaceAll(_startTime, " ", SPACE));
    url.append("'");

    _endTime = _endEdit->date().toString("yyyy-MM-dd").toStdString();
    _endTime.append(" ");
    _endTime.append(_endEdit->time().toString("hh:mm").toStdString());
    url.append(STOP_TIME);
    url.append("'");
    url.append(replaceAll(_endTime, " ", SPACE));
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

    return url;
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
        return HorizonsDialog::HorizonsResult::ConnectionError;
    }

    loop.exec(QEventLoop::ExcludeUserInputEvents);

    return handleReply(reply);
}

HorizonsDialog::HorizonsResult HorizonsDialog::handleReply(QNetworkReply* reply) {
    if (reply->error()) {
        std::cout << reply->errorString().toStdString();
        reply->deleteLater();
        return HorizonsDialog::HorizonsResult::ConnectionError;
    }

    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (redirect.isValid() && reply->url() != redirect) {
        std::cout << "Redirect has been requested" << std::endl;
    }
    auto statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
    if (statusCode.isValid() && statusCode != 200) {
        std::cout << "HTTP status code '" << statusCode.toString().toStdString() << "' was returned" << std::endl;
        return HorizonsDialog::HorizonsResult::ConnectionError;
    }

    QString answer = reply->readAll();
    reply->deleteLater();

    std::cout << "Reply: '" << std::endl;
    std::cout << answer.toStdString();
    std::cout << "'" << std::endl;

    // Convert the answer to a json object and validate it
    json jsonAnswer = json::parse(answer.toStdString());
    HorizonsDialog::HorizonsResult isValid = isValidAnswer(jsonAnswer);
    if (isValid != HorizonsDialog::HorizonsResult::Valid) {
        return isValid;
    }

    // Create a text file and write reply to it
    QString filePathQ = _directoryEdit->text();
    filePathQ.append(QDir::separator());
    filePathQ.append(_nameEdit->text());
    std::string filePath = filePathQ.toStdString();
    std::filesystem::path fullFilePath = std::filesystem::absolute(filePath);

    auto result = jsonAnswer.find("result");
    if (result == jsonAnswer.end()) {
        return HorizonsDialog::HorizonsResult::UnknownError;
    }

    // Check if the file already exists
    if (std::filesystem::is_regular_file(fullFilePath)) {
        return HorizonsDialog::HorizonsResult::FileAlreadyExist;
    }

    // Write response into a new file
    std::ofstream file(filePath);
    file << replaceAll(*result, "\\n", "\n") << std::endl;
    //file << *result << std::endl;
    file.close();

    _horizonsFile = fullFilePath;
    return isValidHorizonsFile(filePath);
}

HorizonsDialog::HorizonsResult HorizonsDialog::isValidAnswer(const json& answer) const {
    auto it = answer.find("error");
    if (it != answer.end()) {
        // There was an error
        std::cout << "Error: " << *it << std::endl;
        return HorizonsDialog::HorizonsResult::UnknownError;
    }
    return HorizonsDialog::HorizonsResult::Valid;
}

// Check whether the given Horizons file is valid or not
// Return an error code with what is the problem if there was one
HorizonsDialog::HorizonsResult HorizonsDialog::isValidHorizonsFile(const std::string& file) const {
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        return HorizonsDialog::HorizonsResult::FileEmpty;
    }

    // The header of a Horizons file has a lot of information about the
    // query that can tell us if the file is valid or not.
    // The line $$SOE indicates start of data.
    std::string line;
    bool foundTarget = false;
    while (fileStream.good() && line.find("$$SOE") == std::string::npos) {
        // Valid Target?
        if (line.find("Revised") != std::string::npos) {
            // If the target is valid, the first line is the date it was last revised
            foundTarget = true;
        }

        // Selected time range too big and step size too small?
        if (line.find("change step-size") != std::string::npos) {
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorSize;
        }

        // Outside valid time range?
        if (line.find("No ephemeris for target") != std::string::npos) {
            // Available time range is located several lines before this in the file
            // The avalable time range is persed later
            std::cout << "The selected time range is outside the valid range for the target"
                << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorTimeRange;
        }

        // Valid Observer?
        if (line.find("No site matches") != std::string::npos ||
            line.find("Cannot find central body") != std::string::npos)
        {
            std::cout << "No matching observer found" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorNoObserver;
        }

        // Are observer and target the same?
        if (line.find("disallowed") != std::string::npos)
        {
            std::cout << "Observer and Target are the same" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorObserverTargetSame;
        }

        // Enough data?
        if (line.find("Insufficient ephemeris data") != std::string::npos)
        {
            std::cout << "Not enough data for the request" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorNoData;
        }

        // Incorrect Observer type?
        if (line.find("Multiple matching stations found") != std::string::npos) {
            // Stations are not supported
            // This message is only shown when a station is entered as observer
            std::cout << "Attempted to use an observer station, NOT supported" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorIncorrectObserver;
        }

        // Multiple matching major bodies?
        if (line.find("Multiple major-bodies match string") != std::string::npos) {
            // Target
            if (!foundTarget) {
                // If target was not found then it is the target that has multiple matches
                std::cout << "Target has multiple matches" << std::endl;
                fileStream.close();
                return HorizonsDialog::HorizonsResult::MultipleTarget;
            }
            // Observer
            else {
                std::cout << "Observer has multiple matches" << std::endl;
                fileStream.close();
                return HorizonsDialog::HorizonsResult::MultipleObserver;
            }
        }

        // Multiple matching small bodies?
        if (line.find("Small-body Index Search Results") != std::string::npos) {
            // Small bodies can only be targets not observers
            fileStream.close();
            return HorizonsDialog::HorizonsResult::MultipleTarget;
        }

        // No Target?
        if (line.find("No matches found") != std::string::npos) {
            std::cout << "No matches found for target" << std::endl;
            fileStream.close();
            return HorizonsDialog::HorizonsResult::ErrorNoTarget;
        }

        std::getline(fileStream, line);
    }

    // If we reached end of file before we found the start of data then it is
    // not a valid file
    if (fileStream.good()) {
        fileStream.close();
        return HorizonsDialog::HorizonsResult::Valid;
    }
    else {
        fileStream.close();
        return HorizonsDialog::HorizonsResult::UnknownError;
    }
}

bool HorizonsDialog::handleResult(HorizonsDialog::HorizonsResult& result) {
    std::string message;
    switch (result) {
        case HorizonsDialog::HorizonsResult::Valid:
            std::cout << "Valid result" << std::endl;
            return true;
        case HorizonsDialog::HorizonsResult::FileEmpty:
            message = "The received horizons file is empty";
            break;
        case HorizonsDialog::HorizonsResult::FileAlreadyExist:
            message = "File already exist, try another filename";
            break;
        case HorizonsDialog::HorizonsResult::ConnectionError:
            message = "Connection error";
            break;

        case HorizonsDialog::HorizonsResult::ErrorSize:
            message = "Time range '" + _startTime + "' to '" + _endTime +
                "' with step size '" + _stepEdit->text().toStdString() +
                "' is too big, try to increase the step size and/or decrease "
                "the time range";
            break;
        case HorizonsDialog::HorizonsResult::ErrorTimeRange: {
            std::pair<std::string, std::string> validTimeRange =
                parseValidTimeRange(_horizonsFile);
            if (validTimeRange.first.empty()) {
                message = "Could not parse the valid time range";
                break;
            }
            message = "Time range is outside the valid range for target '"
                + _targetName + "'. Valid time range '" + validTimeRange.first + "' to '" +
                validTimeRange.second + "'.";
            break;
        }
        case HorizonsDialog::HorizonsResult::ErrorNoObserver:
            message = "No match was found for observer '" + _observerName + "'. "
                "Use '@" + _observerName + "' as observer to list possible matches.";
            break;
        case HorizonsDialog::HorizonsResult::ErrorObserverTargetSame:
            message = "The observer '" + _observerName + "' and target '" + _targetName +
                "' are the same. Please use another observer for the current target.";
            break;
        case HorizonsDialog::HorizonsResult::ErrorNoData:
            message = "There is not enough data to compute the state of target '" +
                _targetName + "' in relation to the observer '" + _observerName +
                "' for the time range '" + _startTime + "' to '" + _endTime +
                "'. Try to use another observer for the current target or another time range.";
            break;
        case HorizonsDialog::HorizonsResult::ErrorIncorrectObserver:
            message = "Incorrect observer type for '" + _observerName + "'. "
                "Use '@" + _observerName + "' as observer to search for alternatives.";
            break;

        case HorizonsDialog::HorizonsResult::MultipleObserver: {
            message = "Multiple matches were found for observer '" +
                _observerName + "'";
            std::map<int, std::string> matchingObservers =
                parseBodies(_horizonsFile);
            if (matchingObservers.empty()) {
                message += ". Could not parse the matching observers.";
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
            message = "No match was found for target '" + _targetName + "'";
            break;
        case HorizonsDialog::HorizonsResult::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data: (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            message = "Multiple matches was found for target '" + _targetName + "'";
            std::map<int, std::string> matchingTargets =
                parseBodies(_horizonsFile);
            if (matchingTargets.empty()) {
                message += ". Could not parse the matching targets";
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

        case HorizonsDialog::HorizonsResult::UnknownError:
            message = "An unknown error occured";
            break;
        default:
            message = "Unknown result type";
            break;
    }

    _errorMsg->setText(message.c_str());
    std::filesystem::remove(_horizonsFile);
    return false;
}

// When the user presses the 'Save' button
void HorizonsDialog::approved() {
    _downloadLabel->show();
    bool result = handleRequest();
    _downloadLabel->hide();
    if (!result || !std::filesystem::is_regular_file(_horizonsFile)) {
        return;
    }
    accept();
}
