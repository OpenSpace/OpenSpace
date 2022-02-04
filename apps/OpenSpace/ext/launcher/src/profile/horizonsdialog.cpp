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
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
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
}

std::filesystem::path HorizonsDialog::file() const {
    return _horizonsFile;
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* wholeLayout = new QHBoxLayout(this);
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* generateLabel = new QLabel("Generate a new Horizons file:", this);
        generateLabel->setObjectName("heading");
        layout->addWidget(generateLabel);

        QLabel* infoLabel = new QLabel("<p>For more information about the Horizons system "
            "please visit: <a href=\"https://ssd.jpl.nasa.gov/horizons/\">"
            "https://ssd.jpl.nasa.gov/horizons/</a></p>",
            this
        );
        infoLabel->setWordWrap(true);
        infoLabel->setObjectName("url");
        infoLabel->setOpenExternalLinks(true);
        layout->addWidget(infoLabel);
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
    wholeLayout->addLayout(layout);
    {
        _log = new QPlainTextEdit(this);
        _log->setReadOnly(true);
        _log->setObjectName("log");

        QPalette p = _log->palette();
        p.setColor(QPalette::All, QPalette::Base, Qt::black);
        p.setColor(QPalette::All, QPalette::Text, Qt::white);

        _log->setPalette(p);
        wholeLayout->addWidget(_log);

        appendLog("Horizons log messages:", HorizonsDialog::LogLevel::None);
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
    _errorMsg->clear();

    std::string url = constructUrl();

    _chooseObserverCombo->clear();
    _chooseObserverCombo->hide();
    _chooseTargetCombo->clear();
    _chooseTargetCombo->hide();

    json answer = sendRequest(url);
    if (answer.empty()) {
        _errorMsg->setText("Connection error");
        return false;
    }

    std::filesystem::path file = handleAnswer(answer);
    if (!std::filesystem::is_regular_file(file)) {
        return false;
    }

    _horizonsFile = file;
    openspace::HorizonsFile horizonsFile(_horizonsFile);
    openspace::HorizonsFile::HorizonsResult result =
        horizonsFile.isValidHorizonsFile();

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
    // In the case of arcseconds, first option
    else if (_timeTypeCombo->currentIndex() == 0) {
        if (60 > _stepEdit->text().toInt() || _stepEdit->text().toInt() > 3600) {
            isValid = false;
            message = "Angular step size need to be in range 60 to 3600";
        }
    }

    if (!message.empty()) {
        _errorMsg->setText(message.c_str());
    }
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
json HorizonsDialog::sendRequest(const std::string url) {
    QNetworkRequest request;
    request.setHeader(QNetworkRequest::UserAgentHeader, "OpenSpace");
    request.setUrl(QUrl(url.c_str()));

    QNetworkReply* reply = _manager->get(request);
    QEventLoop loop;
    auto status = QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    if (!status) {
        appendLog(
            "Could not connect to Horizons API",
            HorizonsDialog::LogLevel::Error
        );
        return json();
    }

    loop.exec(QEventLoop::ExcludeUserInputEvents);

    return handleReply(reply);
}

json HorizonsDialog::handleReply(QNetworkReply* reply) {
    if (reply->error()) {
        QVariant statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
        if (!checkHttpStatus(statusCode)) {
            appendLog(
                "Connection Error: " + reply->errorString().toStdString(),
                HorizonsDialog::LogLevel::Error
            );
        }

        reply->deleteLater();
        return false;
    }

    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (redirect.isValid() && reply->url() != redirect) {
        appendLog(
            "Redirecting request to: " + redirect.toString().toStdString(),
            HorizonsDialog::LogLevel::Info
        );
        return sendRequest(redirect.toString().toStdString());
    }

    QString answer = reply->readAll();
    reply->deleteLater();

    std::cout << "Reply: '" << std::endl;
    std::cout << answer.toStdString();
    std::cout << "'" << std::endl;

    // Convert the answer to a json object and return it
    return json::parse(answer.toStdString());
}

bool HorizonsDialog::checkHttpStatus(const QVariant& statusCode) {
    bool isKnown = true;
    if (statusCode.isValid() && statusCode.toInt() != int(HorizonsDialog::HTTPCodes::Ok)) {
        std::string message;
        int code = statusCode.toInt();

        switch (code) {
            case int(HorizonsDialog::HTTPCodes::BadRequest):
                message = "The request contained invalid keywords and/or content or used "
                "a method other than GET or POST";
                break;
            case int(HorizonsDialog::HTTPCodes::MethodNotAllowed):
                message = "The request used an incorrect method";
                break;
            case int(HorizonsDialog::HTTPCodes::InternalServerError):
                message = "The database is currently not available, try again at a "
                "later time";
                break;
            case int(HorizonsDialog::HTTPCodes::ServiceUnavailable):
                message = "The server is currently unable to handle the request due to a "
                "temporary overloading or maintenance of the server, try again at a "
                "later time";
                break;
            default:
                message = "HTTP status code '" + statusCode.toString().toStdString() +
                    "' was returned";
                isKnown = false;
                break;
        }

        appendLog(message, HorizonsDialog::LogLevel::Error);
    }
    return isKnown;
}

std::filesystem::path HorizonsDialog::handleAnswer(json& answer) {
    openspace::HorizonsFile::HorizonsResult isValid = openspace::HorizonsFile::isValidAnswer(answer);
    if (isValid != openspace::HorizonsFile::HorizonsResult::Valid) {
        handleResult(isValid);
        return std::filesystem::path();
    }

    // Create a text file and write reply to it
    QString filePathQ = _directoryEdit->text();
    filePathQ.append(QDir::separator());
    filePathQ.append(_nameEdit->text());
    std::string filePath = filePathQ.toStdString();
    std::filesystem::path fullFilePath = std::filesystem::absolute(filePath);

    auto result = answer.find("result");
    if (result == answer.end()) {
        _errorMsg->setText("Malformed answer recieved");
        return std::filesystem::path();
    }

    // Check if the file already exists
    if (std::filesystem::is_regular_file(fullFilePath)) {
        _errorMsg->setText("File already exist, try another filename");
        return std::filesystem::path();
    }

    // Write response into a new file
    std::ofstream file(filePath);
    file << replaceAll(*result, "\\n", "\n") << std::endl;
    file.close();

    return fullFilePath;
}

bool HorizonsDialog::handleResult(openspace::HorizonsFile::HorizonsResult& result) {
    switch (result) {
        case openspace::HorizonsFile::HorizonsResult::Valid:
            return true;

        case openspace::HorizonsFile::HorizonsResult::Empty:
            _errorMsg->setText("The horizons file is empty");
            break;

        case openspace::HorizonsFile::HorizonsResult::ErrorSize: {
            std::string message = "Time range '" + _startTime + "' to '" + _endTime +
                "' with step size '" + _stepEdit->text().toStdString() +
                "' " + _timeTypeCombo->currentText().toStdString() +
                " is too big, try to increase the step size and/or decrease "
                "the time range";
            appendLog(message, HorizonsDialog::LogLevel::Error);
            break;
        }

        case openspace::HorizonsFile::HorizonsResult::ErrorTimeRange: {
            appendLog("Time range is outside the valid range for target '"
                + _targetName + "'.", HorizonsDialog::LogLevel::Error);

            std::pair<std::string, std::string> validTimeRange =
                parseValidTimeRange(_horizonsFile);
            if (validTimeRange.first.empty()) {
                appendLog(
                    "Could not parse the valid time range from file",
                    HorizonsDialog::LogLevel::Error
                );
                break;
            }

            appendLog("Valid time range is '" + validTimeRange.first + "' to '" +
                validTimeRange.second + "'", HorizonsDialog::LogLevel::Info);
            break;
        }

        case openspace::HorizonsFile::HorizonsResult::ErrorNoObserver:
            appendLog("No match was found for observer '" + _observerName + "'. "
                "Use '@" + _observerName + "' as observer to list possible matches.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::HorizonsResult::ErrorObserverTargetSame:
            appendLog("The observer '" + _observerName + "' and target '" + _targetName +
                "' are the same. Please use another observer for the current target.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::HorizonsResult::ErrorNoData:
            appendLog("There is not enough data to compute the state of target '" +
                _targetName + "' in relation to the observer '" + _observerName +
                "' for the time range '" + _startTime + "' to '" + _endTime +
                "'. Try to use another observer for the current target or another time range.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::HorizonsResult::MultipleObserverStations:
            appendLog("Multiple observer stations was found for observer '" +
                _observerName + "'. ", HorizonsDialog::LogLevel::Warning
            );
            appendLog("Did not find what you were looking for? Use '@" + _observerName +
                "' as observer to search for alternatives.",
                HorizonsDialog::LogLevel::Info
            );
            break;

        case openspace::HorizonsFile::HorizonsResult::MultipleObserver: {
            appendLog("Multiple matches were found for observer '" +
                _observerName + "'",
                HorizonsDialog::LogLevel::Warning
            );

            std::map<int, std::string> matchingObservers =
                parseBodies(_horizonsFile);
            if (matchingObservers.empty()) {
                appendLog("Could not parse the matching observers",
                    HorizonsDialog::LogLevel::Error
                );
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

        case openspace::HorizonsFile::HorizonsResult::ErrorNoTarget:
            appendLog("No match was found for target '" + _targetName + "'",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::HorizonsResult::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data: (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            appendLog("Multiple matches was found for target '" + _targetName + "'",
                HorizonsDialog::LogLevel::Warning
            );

            std::map<int, std::string> matchingTargets =
                parseBodies(_horizonsFile);
            if (matchingTargets.empty()) {
                appendLog("Could not parse the matching targets",
                    HorizonsDialog::LogLevel::Error
                );
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

        case openspace::HorizonsFile::HorizonsResult::UnknownError:
            _errorMsg->setText("An unknown error occured");
            break;

        default:
            _errorMsg->setText("Unknown result type");
            break;
    }

    std::filesystem::remove(_horizonsFile);
    return false;
}

void HorizonsDialog::appendLog(const std::string& message, const LogLevel level) {
    std::string htmlText;
    switch (level)
    {
        case HorizonsDialog::LogLevel::Error:
            htmlText = "<font color=\"Red\">(E)  ";
            break;
        case HorizonsDialog::LogLevel::Warning:
            htmlText = "<font color=\"Yellow\">(W)  ";
            break;
        case HorizonsDialog::LogLevel::Info:
            htmlText = "<font color=\"White\">(I)  ";
            break;
        case HorizonsDialog::LogLevel::None:
        default:
            htmlText = "<font color=\"White\">";
            break;
    }

    htmlText += message + "</font>";
    _log->appendHtml(htmlText.c_str());
    _log->verticalScrollBar()->setValue(_log->verticalScrollBar()->maximum());
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
