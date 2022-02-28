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
    // Text for the different units for the step size
    constexpr const char* TimeVarying = "arcseconds (time-varying)";
    constexpr const char* Minutes = "minutes";
    constexpr const char* Hours = "hours";
    constexpr const char* Days = "days";
    constexpr const char* Months = "calendar months";
    constexpr const char* Years = "calendar years";
    constexpr const char* Unitless = "equal intervals (unitless)";

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
                result = result.insert(start, " | ");

                step = 1;
                offset = start;
            }

            // Keep looking
            offset = start + step;
            step = 1;
        }

        return result;
    }

    int findId(const std::string& match) {
        // Format: id, other information...
        std::stringstream str(match);
        int id;

        str >> id;
        return id;
    }
} // namespace

HorizonsDialog::HorizonsDialog(QWidget* parent)
    : QDialog(parent)
{
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    _manager = new QNetworkAccessManager(this);

    setWindowTitle("Horizons");
    createWidgets();
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

#ifdef OPENSPACE_MODULE_SPACE_ENABLED

std::filesystem::path HorizonsDialog::file() const {
    return _horizonsFile.file();
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* wholeLayout = new QHBoxLayout(this);
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* generateLabel = new QLabel("Generate a new Horizons file", this);
        generateLabel->setObjectName("heading");
        layout->addWidget(generateLabel);

        QLabel* infoLabel = new QLabel("<p>For more information about the Horizons "
            "system please visit: <a href=\"https://ssd.jpl.nasa.gov/horizons/\">"
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
        QLabel* typeLabel = new QLabel("Horizons data type:", this);
        container->addWidget(typeLabel);

        _typeCombo = new QComboBox(this);
        _typeCombo->setToolTip("Choose Horizons data type");
        QStringList types = {
           "Vector table",
           "Observer table"
        };
        _typeCombo->addItems(types);
        _typeCombo->setCurrentIndex(0);
        container->addWidget(_typeCombo);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* nameLabel = new QLabel("Filename:", this);
        container->addWidget(nameLabel);

        _nameEdit = new QLineEdit(QString::fromStdString("horizons.dat"), this);
        _nameEdit->setToolTip(
            "Name of the generated Horizons file. Must end with '.dat'"
        );
        container->addWidget(_nameEdit);

        layout->addLayout(container);
    }
    {
        QBoxLayout* container = new QHBoxLayout(this);
        QLabel* directoryLabel = new QLabel("Save directory:", this);
        container->addWidget(directoryLabel);

        _directoryEdit = new QLineEdit(this);
        _directoryEdit->setToolTip(
            "Directory where the generated Horizons file is saved"
        );
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

        _targetEdit =
            new QLineEdit(QString::fromStdString("Mars Reconnaissance Orbiter"), this);
        _targetEdit->setToolTip(
            "Which target or body would you like Horizons trajectery data for?"
        );
        container->addWidget(_targetEdit);

        layout->addLayout(container);

        _chooseTargetCombo = new QComboBox(this);
        _chooseTargetCombo->hide();
        _chooseTargetCombo->setToolTip(
            "Choose a target from the search, or search again"
        );
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
        _chooseObserverCombo->setToolTip(
            "Choose an observer from the search, or search again"
        );
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
            TimeVarying,
            Minutes,
            Hours,
            Days,
            Months,
            Years,
            Unitless
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

    openspace::HorizonsFile file = handleAnswer(answer);
    if (file.isEmpty()) {
        return false;
    }

    _horizonsFile = std::move(file);
    openspace::HorizonsFile::ResultCode result =
        _horizonsFile.isValidHorizonsFile();

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
    // Check if input is numerical
    bool couldConvert = false;
    int32_t step = _stepEdit->text().toInt(&couldConvert);
    if (!couldConvert) {
        isValid = false;
        message = "Step size needs to be a number in range 1 to " +
            std::to_string(std::numeric_limits<int32_t>::max());
    }
    else {
        // In the case of arcseconds (first option) range is different
        if (_timeTypeCombo->currentIndex() == 0) {
            if (60 > step || step > 3600) {
                isValid = false;
                message = "Angular step size needs to be in range 60 to 3600";
            }
        }
        // Numbers only and in range 1 to 2147483647 (max of 32 bit int)
        // Horizons read the step size into a 32 bit int, but verifies the input on their
        // website as a uint32_t. If step size over 32 bit int is sent, this error
        // message is recived: Cannot read numeric value -- re-enter
        else if (1 > step || step > std::numeric_limits<int32_t>::max()) {
            isValid = false;
            message = "Step size is outside valid range 1 to " + std::to_string(std::numeric_limits<int32_t>::max());
        }
    }

    if (!message.empty()) {
        _errorMsg->setText(message.c_str());
    }
    return isValid;
}

std::string HorizonsDialog::constructUrl() {
    // Construct url for request
    openspace::HorizonsFile::Type type;
    if (_typeCombo->currentIndex() == 0) {
        type = openspace::HorizonsFile::Type::Vector;
    }
    else if (_typeCombo->currentIndex() == 1) {
        type = openspace::HorizonsFile::Type::Observer;
    }
    else {
        _errorMsg->setText("Invalid Horizons type");
        return "";
    }

    std::string command;
    if (_chooseTargetCombo->count() > 0 && _chooseTargetCombo->currentIndex() != 0) {
        command = _chooseTargetCombo->itemData(_chooseTargetCombo->currentIndex())
            .toString().toStdString();
        _targetName = _chooseTargetCombo->currentText().toStdString();
        _targetEdit->setText(command.c_str());
    }
    else {
        command = _targetEdit->text().toStdString();
        _targetName = command;
    }

    std::string center;
    if (_chooseObserverCombo->count() > 0 && _chooseObserverCombo->currentIndex() != 0) {
        std::string id =_chooseObserverCombo->itemData(
            _chooseObserverCombo->currentIndex()
        ).toString().toStdString();
        center = "@" + id;
        _observerName = _chooseObserverCombo->currentText().toStdString();
        _centerEdit->setText(id.c_str());
    }
    else {
        center = _centerEdit->text().toStdString();
        _observerName = center;
    }

    _startTime = _startEdit->date().toString("yyyy-MM-dd").toStdString();
    _startTime.append(" ");
    _startTime.append(_startEdit->time().toString("hh:mm").toStdString());

    _endTime = _endEdit->date().toString("yyyy-MM-dd").toStdString();
    _endTime.append(" ");
    _endTime.append(_endEdit->time().toString("hh:mm").toStdString());

    std::string unit;
    if (_timeTypeCombo->currentText().toStdString() == TimeVarying) {
        unit = "VAR";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Minutes) {
        unit = "m";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Hours) {
        unit = "h";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Days) {
        unit = "d";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Months) {
        unit = "MO";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Years) {
        unit = "Y";
    }
    else if (_timeTypeCombo->currentText().toStdString() == Unitless) {
        unit = "";
    }
    else {
        _errorMsg->setText("Invalid unit type");
        return "";
    }

    return openspace::HorizonsFile::constructUrl(
        type,
        command,
        center,
        _startTime,
        _endTime,
        _stepEdit->text().toStdString(),
        unit
    );
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
    if (reply->error() != QNetworkReply::NoError) {
        QVariant statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
        if (!checkHttpStatus(statusCode)) {
            appendLog(
                "Connection Error: " + reply->errorString().toStdString(),
                HorizonsDialog::LogLevel::Error
            );
        }

        reply->deleteLater();
        return json();
    }

    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (redirect.isValid() && reply->url() != redirect) {
        if (redirect.isRelative()) {
            redirect = reply->url().resolved(redirect);
        }

        appendLog(
            "Redirecting request to: " + redirect.toString().toStdString(),
            HorizonsDialog::LogLevel::Info
        );
        return sendRequest(redirect.toString().toStdString());
    }

    QString answer = reply->readAll();
    reply->deleteLater();

    if (answer.isEmpty()) {
        appendLog(
            "Connection Error: " + reply->errorString().toStdString(),
            HorizonsDialog::LogLevel::Error
        );
        return json();
    }

    std::cout << "Reply: '" << std::endl;
    std::cout << answer.toStdString();
    std::cout << "'" << std::endl;

    // Convert the answer to a json object and return it
    return json::parse(answer.toStdString());
}

bool HorizonsDialog::checkHttpStatus(const QVariant& statusCode) {
    bool isKnown = true;
    if (statusCode.isValid() &&
        statusCode.toInt() != int(HorizonsDialog::HTTPCodes::Ok))
    {
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
    else if (!statusCode.isValid()) {
        appendLog("HTTP status code is not valid", HorizonsDialog::LogLevel::Error);
        isKnown = false;
    }
    return isKnown;
}

openspace::HorizonsFile HorizonsDialog::handleAnswer(json& answer) {
    openspace::HorizonsFile::ResultCode isValid =
        openspace::HorizonsFile::isValidAnswer(answer);
    if (isValid != openspace::HorizonsFile::ResultCode::Valid &&
        isValid != openspace::HorizonsFile::ResultCode::MultipleObserverStations &&
        isValid != openspace::HorizonsFile::ResultCode::ErrorTimeRange)
    {
        // Special case with MultipleObserverStations since it is detected as an error
        // but could be fixed by parsing the matches and let user choose
        // Special case with ErrorTimeRange since it is detected as an error
        // but could be nice to display the available time range of target to the user
        handleResult(isValid);
        return openspace::HorizonsFile();
    }

    // Create a text file and write reply to it
    QString filePathQ = _directoryEdit->text();
    filePathQ.append(QDir::separator());
    filePathQ.append(_nameEdit->text());
    std::string filePath = filePathQ.toStdString();
    std::filesystem::path fullFilePath = std::filesystem::absolute(filePath);

    auto result = answer.find("result");
    if (result == answer.end()) {
        appendLog(
            "Malformed answer recieved: " + answer.dump(),
            HorizonsDialog::LogLevel::Error
        );
        return openspace::HorizonsFile();
    }

    // Check if the file already exists
    if (std::filesystem::is_regular_file(fullFilePath)) {
        _errorMsg->setText("File already exist, try another filename");
        return openspace::HorizonsFile();
    }

    // Return a new file with the result
    return openspace::HorizonsFile(fullFilePath, *result);
}

bool HorizonsDialog::handleResult(openspace::HorizonsFile::ResultCode& result) {
    switch (result) {
        case openspace::HorizonsFile::ResultCode::Valid:
            return true;

        case openspace::HorizonsFile::ResultCode::Empty:
            _errorMsg->setText("The horizons file is empty");
            break;

        case openspace::HorizonsFile::ResultCode::ErrorSize: {
            std::string message = "Time range '" + _startTime + "' to '" + _endTime +
                "' with step size '" + _stepEdit->text().toStdString() +
                "' " + _timeTypeCombo->currentText().toStdString() +
                " is too big, try to increase the step size and/or decrease "
                "the time range";
            appendLog(message, HorizonsDialog::LogLevel::Error);
            break;
        }

        case openspace::HorizonsFile::ResultCode::ErrorSpan:
            appendLog(
                "Step size is too big, exceeds available time span for target",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::ErrorTimeRange: {
            appendLog("Time range is outside the valid range for target '"
                + _targetName + "'.", HorizonsDialog::LogLevel::Error);

            std::pair<std::string, std::string> validTimeRange =
                _horizonsFile.parseValidTimeRange("Trajectory files", "************");
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

        case openspace::HorizonsFile::ResultCode::ErrorNoObserver:
            appendLog("No match was found for observer '" + _observerName + "'. "
                "Use '@" + _observerName + "' as observer to list possible matches.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::ErrorObserverTargetSame:
            appendLog("The observer '" + _observerName + "' and target '" + _targetName +
                "' are the same. Please use another observer for the current target.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::ErrorNoData:
            appendLog("There is not enough data to compute the state of target '" +
                _targetName + "' in relation to the observer '" + _observerName +
                "' for the time range '" + _startTime + "' to '" + _endTime +
                "'. Try to use another observer for the current target or another "
                "time range.",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::MultipleObserverStations: {
            appendLog("Multiple matching observer stations were found for observer '" +
                _observerName + "'. ", HorizonsDialog::LogLevel::Warning
            );
            appendLog("Did not find what you were looking for? Use '@" + _observerName +
                "' as observer to search for alternatives.",
                HorizonsDialog::LogLevel::Info
            );

            std::vector<std::string> matchingstations =
                _horizonsFile.parseMatches(
                    "Observatory Name",
                    "Multiple matching stations found"
                );
            if (matchingstations.empty()) {
                appendLog("Could not parse the matching stations",
                    HorizonsDialog::LogLevel::Error
                );
                break;
            }
            _chooseObserverCombo->clear();
            for (std::string station : matchingstations) {
                _chooseObserverCombo->addItem(
                    station.c_str(),
                    findId(station)
                );
            }
            _chooseObserverCombo->setCurrentIndex(0);
            _chooseObserverCombo->show();
            break;
        }

        case openspace::HorizonsFile::ResultCode::MultipleObserver: {
            appendLog("Multiple matches were found for observer '" +
                _observerName + "'",
                HorizonsDialog::LogLevel::Warning
            );

            std::vector<std::string> matchingObservers =
                _horizonsFile.parseMatches("Name", "matches");
            if (matchingObservers.empty()) {
                appendLog("Could not parse the matching observers",
                    HorizonsDialog::LogLevel::Error
                );
                break;
            }
            _chooseObserverCombo->clear();
            for (std::string observer : matchingObservers) {
                _chooseObserverCombo->addItem(
                    observer.c_str(),
                    findId(observer)
                );
            }
            _chooseObserverCombo->setCurrentIndex(0);
            _chooseObserverCombo->show();
            break;
        }

        case openspace::HorizonsFile::ResultCode::ErrorNoTarget:
            appendLog("No match was found for target '" + _targetName + "'",
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data:
            // (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            appendLog("Multiple matches was found for target '" + _targetName + "'",
                HorizonsDialog::LogLevel::Warning
            );

            std::vector<std::string> matchingTargets =
                _horizonsFile.parseMatches("Name", "matches");
            if (matchingTargets.empty()) {
                appendLog("Could not parse the matching targets",
                    HorizonsDialog::LogLevel::Error
                );
                break;
            }
            _chooseTargetCombo->clear();
            for (std::string target : matchingTargets) {
                _chooseTargetCombo->addItem(
                    target.c_str(),
                    findId(target)
                );
            }
            _chooseTargetCombo->setCurrentIndex(0);
            _chooseTargetCombo->show();
            break;
        }

        case openspace::HorizonsFile::ResultCode::UnknownError:
            _errorMsg->setText("An unknown error occured");
            break;

        default:
            _errorMsg->setText("Unknown result type");
            break;
    }

    std::filesystem::remove(_horizonsFile.file());
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
    if (!result || !std::filesystem::is_regular_file(_horizonsFile.file())) {
        return;
    }
    accept();
}

#endif // OPENSPACE_MODULE_SPACE_ENABLED
