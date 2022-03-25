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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include "profile/line.h"
#include <QComboBox>
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QDir>
#include <QEventLoop>
#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QPlainTextEdit>
#include <QProgressBar>
#include <QPushButton>
#include <QScrollBar>
#include <QVBoxLayout>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>

namespace {
    constexpr const char* _loggerCat = "HorizonsDialog";

    // Text for the different units for the step size
    constexpr const char* TimeVarying = "arcseconds (time-varying)";
    constexpr const char* Minutes = "minutes";
    constexpr const char* Hours = "hours";
    constexpr const char* Days = "days";
    constexpr const char* Months = "calendar months";
    constexpr const char* Years = "calendar years";
    constexpr const char* Unitless = "equal intervals (unitless)";

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
    QGridLayout* layout = new QGridLayout(this);
    layout->setSpacing(10);
    {
        QLabel* generateLabel = new QLabel("Generate a Horizons file", this);
        generateLabel->setObjectName("heading");
        layout->addWidget(generateLabel, 0, 0, 1, 3);

        QLabel* infoLabel = new QLabel("<p>For more information about the Horizons "
            "system please visit: <a href=\"https://ssd.jpl.nasa.gov/horizons/\">"
            "https://ssd.jpl.nasa.gov/horizons/</a></p>",
            this
        );
        infoLabel->setWordWrap(true);
        infoLabel->setObjectName("thin");
        infoLabel->setOpenExternalLinks(true);
        layout->addWidget(infoLabel, 1, 0, 1, 3);
    }
    {
        _typeLabel = new QLabel("Horizons data type:", this);
        _typeLabel->setToolTip("Choose Horizons data type");
        layout->addWidget(_typeLabel, 2, 0, 1, 2);

        _typeCombo = new QComboBox(this);
        _typeCombo->setToolTip("Choose Horizons data type");
        QStringList types = {
           "Vector table",
           "Observer table"
        };
        _typeCombo->addItems(types);
        _typeCombo->setCurrentIndex(0);
        connect(
            _typeCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &HorizonsDialog::typeOnChange
        );
        layout->addWidget(_typeCombo, 2, 2);
    }
    {
        _fileLabel = new QLabel("File Path:", this);
        _fileLabel->setToolTip(
            "Where the generated Horizons file is saved"
        );
        layout->addWidget(_fileLabel, 3, 0);

        QBoxLayout* container = new QHBoxLayout(this);
        _fileEdit = new QLineEdit(this);
        _fileEdit->setToolTip(
            "Where the generated Horizons file is saved"
        );
        container->addWidget(_fileEdit);

        QPushButton* browseButton = new QPushButton("Browse", this);
        browseButton->setDefault(false);
        connect(
            browseButton,
            &QPushButton::released,
            this,
            &HorizonsDialog::openSaveAs
        );
        browseButton->setCursor(Qt::PointingHandCursor);
        browseButton->setToolTip(
            "Browse for a file path where the generated Horizons file will be saved"
        );
        container->addWidget(browseButton);

        layout->addLayout(container, 3, 1, 1, 2);
    }
    {
        _targetLabel = new QLabel("Target Body:", this);
        _targetLabel->setToolTip(
            "Which target or body would you like Horizons trajectery data for?"
        );
        layout->addWidget(_targetLabel, 4, 0);

        _targetEdit =
            new QLineEdit(QString::fromStdString("Tesla"), this);
        _targetEdit->setToolTip(
            "Which target or body would you like Horizons trajectery data for?"
        );
        layout->addWidget(_targetEdit, 4, 1, 1, 2);

        _chooseTargetCombo = new QComboBox(this);
        _chooseTargetCombo->setObjectName("mono");
        _chooseTargetCombo->hide();
        _chooseTargetCombo->setToolTip(
            "Choose a target from the search, or search again"
        );
        layout->addWidget(_chooseTargetCombo, 5, 1, 1, 2);
    }
    {
        _centerLabel = new QLabel("Observer Location:", this);
        _centerLabel->setToolTip("In which reference frame do you want the data in?");
        layout->addWidget(_centerLabel, 6, 0);

        _centerEdit = new QLineEdit(QString::fromStdString("@ssb"), this);
        _centerEdit->setToolTip("In which reference frame do you want the data in?");
        layout->addWidget(_centerEdit, 6, 1, 1, 2);

        _chooseObserverCombo = new QComboBox(this);
        _chooseObserverCombo->setObjectName("mono");
        _chooseObserverCombo->hide();
        _chooseObserverCombo->setToolTip(
            "Choose an observer from the search, or search again"
        );
        layout->addWidget(_chooseObserverCombo, 7, 1, 1, 2);
    }
    {
        _startLabel = new QLabel("Start Time:", this);
        _startLabel->setToolTip("Enter the start date and time for the data");
        layout->addWidget(_startLabel, 8, 0, 1, 2);
        _startEdit = new QDateTimeEdit(this);
        _startEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _startEdit->setDate(QDate::currentDate().addYears(-1));
        _startEdit->setToolTip("Enter the start date and time for the data");
        layout->addWidget(_startEdit, 8, 2);
    }
    {
        _endLabel = new QLabel("End Time:", this);
        _endLabel->setToolTip("Enter the end date and time for the data");
        layout->addWidget(_endLabel, 9, 0, 1, 2);
        _endEdit = new QDateTimeEdit(this);
        _endEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _endEdit->setDate(QDate::currentDate());
        _endEdit->setToolTip("Enter the end date and time for the data");
        layout->addWidget(_endEdit, 9, 2);
    }
    {
        _importTimeButton = new QPushButton("Import timerange", this);
        _importTimeButton->setDefault(false);
        connect(
            _importTimeButton,
            &QPushButton::released,
            this,
            &HorizonsDialog::importTimeRange
        );
        _importTimeButton->setCursor(Qt::PointingHandCursor);
        _importTimeButton->setToolTip(
            "Import parsed timerange for full data coverage"
        );
        _importTimeButton->hide();
        layout->addWidget(_importTimeButton, 10, 2);
    }
    {
        _stepLabel = new QLabel("Step Size:", this);
        _stepLabel->setToolTip("Enter the step size for the data");
        layout->addWidget(_stepLabel, 11, 0);

        _stepEdit = new QLineEdit(this);
        _stepEdit->setValidator(new QIntValidator(this));
        _stepEdit->setText(QString::number(1));
        _stepEdit->setToolTip("Enter the step size for the data");
        layout->addWidget(_stepEdit, 11, 1);

        _timeTypeCombo = new QComboBox(this);
        _timeTypeCombo->setToolTip("Choose unit of the step size");
        QStringList timeTypes = {
            Minutes,
            Hours,
            Days,
            Months,
            Years,
            Unitless
        };
        _timeTypeCombo->addItems(timeTypes);
        _timeTypeCombo->setCurrentIndex(2);
        layout->addWidget(_timeTypeCombo, 11, 2);
    }
    layout->addWidget(new Line, 12, 0, 1, 3);
    {
        _log = new QPlainTextEdit(this);
        _log->setReadOnly(true);
        _log->setObjectName("log");

        QPalette p = _log->palette();
        p.setColor(QPalette::All, QPalette::Base, Qt::black);
        p.setColor(QPalette::All, QPalette::Text, Qt::white);

        _log->setPalette(p);
        _log->setToolTip("Any error messages from the data request is displayed here");
        layout->addWidget(_log, 13, 0, 1, 3);

        appendLog("Horizons log messages:", HorizonsDialog::LogLevel::None);
    }
    layout->addWidget(new Line, 14, 0, 1, 3);
    {
        _downloadLabel = new QLabel("Downloading file...", this);
        _downloadLabel->setObjectName("thin");
        _downloadLabel->hide();
        layout->addWidget(_downloadLabel, 15, 0);

        _downloadProgress = new QProgressBar(this);
        _downloadProgress->hide();
        layout->addWidget(_downloadProgress, 15, 1, 1, 2);
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
        layout->addLayout(footer, 16, 0, 1, 3);
    }
}

void HorizonsDialog::cleanAllWidgets() {
    styleLabel(_typeLabel, false);
    styleLabel(_fileLabel, false);
    styleLabel(_targetLabel, false);
    styleLabel(_centerLabel, false);
    styleLabel(_startLabel, false);
    styleLabel(_endLabel, false);
    styleLabel(_stepLabel, false);
}

void HorizonsDialog::styleLabel(QLabel* label, bool isDirty) {
    std::string newStyle;
    if (isDirty) {
        newStyle = "error";
    }
    else {
        newStyle = "normal";
    }
    label->setObjectName(newStyle.c_str());
    label->style()->unpolish(label);
    label->style()->polish(label);
}

void HorizonsDialog::openSaveAs() {
    std::string filename = QFileDialog::getSaveFileName(
        this,
        "Choose a file path where the generated Horizons file will be saved",
        absPath("${USER}").string().c_str(),
        "Horizons data file (*.hrz);;(*.hrz)",
        nullptr
#ifdef __linux__
        , QFileDialog::DontUseNativeDialog
#endif
    ).toStdString();
    _fileEdit->setText(filename.c_str());
}

void HorizonsDialog::typeOnChange(int index) {
    // Vector table type doesn't support time varying or arcseconds time steps
    if (index == 0 && _timeTypeCombo->itemText(0) == TimeVarying) {
        _timeTypeCombo->removeItem(0);
    }
    // Observer
    else if (index == 1 && _timeTypeCombo->itemText(0) != TimeVarying) {
        _timeTypeCombo->insertItem(0, TimeVarying);
    }
    else {
        _errorMsg->setText("Invalid Horizons type");
        styleLabel(_typeLabel, true);
    }
}

void HorizonsDialog::downloadProgress(qint64 value, qint64 total) {
    if (total < 0) {
        _downloadProgress->setRange(0, 0);
        return;
    }
    _downloadProgress->setRange(0, total);
    _downloadProgress->setValue(value);
}

void HorizonsDialog::importTimeRange() {
    QDateTime start, end;
    start = QDateTime::fromString(_validTimeRange.first.c_str());
    end = QDateTime::fromString(_validTimeRange.second.c_str());

    if (!start.isValid() || !end.isValid()) {
        QDate startDate = QDate::fromString(_validTimeRange.first.c_str(), "yyyy-MMM-dd");
        QDate endDate = QDate::fromString(_validTimeRange.second.c_str(), "yyyy-MMM-dd");

        if (startDate.isValid() && endDate.isValid()) {
            _startEdit->setDate(startDate);
            _endEdit->setDate(endDate);
            _importTimeButton->hide();
            _validTimeRange = std::pair<std::string, std::string>();
            return;
        }

        _errorMsg->setText("Could not import time range");
        appendLog(fmt::format("Could not import time range '{}' to '{}'",
            _validTimeRange.first, _validTimeRange.second), LogLevel::Error
        );
        return;
    }

    _startEdit->setDateTime(start);
    _endEdit->setDateTime(end);
    _importTimeButton->hide();
    _validTimeRange = std::pair<std::string, std::string>();
}

std::pair<std::string, std::string> HorizonsDialog::readTimeRange() {
    std::pair<std::string, std::string> timeRange = _horizonsFile.parseValidTimeRange(
        "Trajectory files",
        "************",
        "Trajectory name"
    );

    QDateTime start, end;
    start = QDateTime::fromString(timeRange.first.c_str(), "yyyy-MMM-dd T hh:mm");
    end = QDateTime::fromString(timeRange.second.c_str(), "yyyy-MMM-dd T hh:mm");

    if (!start.isValid() || !end.isValid()) {
        start = QDateTime::fromString(timeRange.first.c_str(), "yyyy-MMM-dd T hh:mm:ss");
        end = QDateTime::fromString(timeRange.second.c_str(), "yyyy-MMM-dd T hh:mm:ss");

        if (!start.isValid() || !end.isValid()) {
            timeRange = _horizonsFile.parseValidTimeRange(
                "Trajectory files",
                "************",
                "Trajectory name",
                false
            );

            start = QDateTime::fromString(timeRange.first.c_str(), "yyyy-MMM-dd");
            end = QDateTime::fromString(timeRange.second.c_str(), "yyyy-MMM-dd");

            if (!start.isValid() || !end.isValid()) {
                if (timeRange.first.empty() || timeRange.second.empty()) {
                    _errorMsg->setText("Could not find time range");
                    appendLog(
                        "Could not find time range in Horizons file",
                        LogLevel::Error
                    );
                }
                else {
                    _errorMsg->setText("Could not read time range");
                    appendLog(fmt::format("Could not read time range '{}' to '{}'",
                        timeRange.first, timeRange.second), LogLevel::Error
                    );
                }
                return std::pair<std::string, std::string>();
            }
            else {
                appendLog(
                    fmt::format("Could not find all time range information. Latest "
                        "Horizons mesage: {}", _latestHorizonsError
                    ),
                    LogLevel::Warning
                );
            }
        }
    }
    return timeRange;
}

bool HorizonsDialog::handleRequest() {
    if (!isValidInput()) {
        return false;
    }

    // Reset
    _errorMsg->clear();
    cleanAllWidgets();
    _importTimeButton->hide();
    _validTimeRange = std::pair<std::string, std::string>();
    _latestHorizonsError.clear();

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

    bool isValid = handleResult(result);

    if (!isValid && std::filesystem::is_regular_file(_horizonsFile.file())) {
        std::string newName = _horizonsFile.file().filename().string();
        newName = newName.substr(0, newName.size() - newName.find_first_of('.'));

        std::filesystem::path oldFile = _horizonsFile.file();
        std::filesystem::path newFile =_horizonsFile.file().replace_filename(
            newName + "_error.txt"
        );

        std::filesystem::rename(oldFile, newFile);
    }

    return isValid;
}

bool HorizonsDialog::isValidInput() {
    bool isValid = true;
    std::string message;

    // File
    if (_fileEdit->text().isEmpty()) {
        isValid = false;
        message = "File path not selected";
        styleLabel(_fileLabel, true);
    }

    // Target field
    else if (_targetEdit->text().isEmpty() &&
            (_chooseTargetCombo->count() > 0 &&
             _chooseTargetCombo->currentIndex() == 0))
    {
        isValid = false;
        message = "Target not selected";
        styleLabel(_targetLabel, true);
    }
    else if (_targetEdit->text().isEmpty() && _chooseTargetCombo->count() == 0) {
        isValid = false;
        message = "Target not selected";
        styleLabel(_targetLabel, true);
    }

    // Observer field
    else if (_centerEdit->text().isEmpty() &&
            (_chooseObserverCombo->count() > 0 &&
             _chooseObserverCombo->currentIndex() == 0))
    {
        isValid = false;
        message = "Observer not selected";
        styleLabel(_centerLabel, true);
    }
    else if (_centerEdit->text().isEmpty() && _chooseObserverCombo->count() == 0) {
        isValid = false;
        message = "Observer not selected";
        styleLabel(_centerLabel, true);
    }

    // Step size
    else if (_stepEdit->text().isEmpty()) {
        isValid = false;
        message = "Step size not selected";
        styleLabel(_stepLabel, true);
    }
    // Check if input is numerical
    bool couldConvert = false;
    int32_t step = _stepEdit->text().toInt(&couldConvert);
    if (!couldConvert) {
        isValid = false;
        message = "Step size needs to be a number in range 1 to " +
            std::to_string(std::numeric_limits<int32_t>::max());
        styleLabel(_stepLabel, true);
    }
    else {
        // In the case of arcseconds range is different
        if (_timeTypeCombo->currentText() == TimeVarying) {
            if (60 > step || step > 3600) {
                isValid = false;
                message = "Angular step size needs to be in range 60 to 3600";
                styleLabel(_stepLabel, true);
            }
        }
        // Numbers only and in range 1 to 2147483647 (max of 32 bit int).
        // Horizons read the step size into a 32 bit int, but verifies the input on their
        // website as a uint32_t. If step size over 32 bit int is sent, this error
        // message is recived: Cannot read numeric value -- re-enter
        else if (1 > step || step > std::numeric_limits<int32_t>::max()) {
            isValid = false;
            message = fmt::format("Step size is outside valid range 1 to '{}'",
                std::to_string(std::numeric_limits<int32_t>::max()));
            styleLabel(_stepLabel, true);
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
        styleLabel(_typeLabel, true);
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
    _startTime.append(_startEdit->time().toString("hh:mm:ss").toStdString());

    _endTime = _endEdit->date().toString("yyyy-MM-dd").toStdString();
    _endTime.append(" ");
    _endTime.append(_endEdit->time().toString("hh:mm:ss").toStdString());

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
        _errorMsg->setText("Invalid time unit type");
        styleLabel(_stepLabel, true);
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
    connect(
        reply,
        &QNetworkReply::downloadProgress,
        this,
        &HorizonsDialog::downloadProgress
    );
    _downloadProgress->reset();
    _downloadProgress->show();

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
    _downloadProgress->hide();
    if (reply->error() != QNetworkReply::NoError) {
        QVariant statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
        if (!checkHttpStatus(statusCode)) {
            appendLog(
                fmt::format("Connection Error '{}' ", reply->errorString().toStdString()),
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
            fmt::format("Redirecting request to '{}'", redirect.toString().toStdString()),
            HorizonsDialog::LogLevel::Info
        );
        return sendRequest(redirect.toString().toStdString());
    }

    QString answer = reply->readAll();
    reply->deleteLater();

    if (answer.isEmpty()) {
        appendLog(
            fmt::format("Connection Error '{}'", reply->errorString().toStdString()),
            HorizonsDialog::LogLevel::Error
        );
        return json();
    }

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
                message = "The request contained invalid keywords and/or used "
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
                message = fmt::format("HTTP status code '{}' was returned",
                    statusCode.toString().toStdString());
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
    auto it = answer.find("error");
    if (it != answer.end()) {
        _latestHorizonsError = *it;
    }

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
    std::filesystem::path filePath =
        std::filesystem::absolute(_fileEdit->text().toStdString());

    auto result = answer.find("result");
    if (result == answer.end()) {
        appendLog(
            fmt::format("Malformed answer recieved '{}'", answer.dump()),
            HorizonsDialog::LogLevel::Error
        );
        return openspace::HorizonsFile();
    }

    // Check if the file already exists
    if (std::filesystem::is_regular_file(filePath)) {
        QMessageBox msgBox;
        msgBox.setText("File already exist \nDo you want to replace it?");
        msgBox.setStandardButtons(
            QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel
        );
        msgBox.setDefaultButton(QMessageBox::Yes);
        int ret = msgBox.exec();
        switch (ret) {
            case QMessageBox::Yes:
                // If yes then continue as normal
                break;
            case QMessageBox::No:
            case QMessageBox::Cancel:
                _errorMsg->setText("File already exist, try another file path");
                styleLabel(_fileLabel, true);
                return openspace::HorizonsFile();
            default:
                _errorMsg->setText("Invalid answer");
                styleLabel(_fileLabel, true);
                return openspace::HorizonsFile();
        }
    }

    // Return a new file with the result
    return openspace::HorizonsFile(filePath, *result);
}

bool HorizonsDialog::handleResult(openspace::HorizonsFile::ResultCode& result) {
    switch (result) {
    case openspace::HorizonsFile::ResultCode::Valid: {
            // If the request worked then delete the corresponding error file if it exist
            std::filesystem::path validFile(_horizonsFile.file());

            std::string errorName = validFile.filename().string();
            errorName =
                errorName.substr(0, errorName.size() - errorName.find_first_of('.'));

            std::filesystem::path errorFile = validFile.replace_filename(
                errorName + "_error.txt"
            );

            if (std::filesystem::is_regular_file(errorFile)) {
                std::filesystem::remove(errorFile);
            }
            return true;
        }
        case openspace::HorizonsFile::ResultCode::InvalidFormat:
            appendLog(fmt::format("Format of file '{}' is invalid. Horizons files must "
                "have extension '.hrz'", _horizonsFile.file()),
                LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::Empty:
            _errorMsg->setText("The horizons file is empty");
            if (!_latestHorizonsError.empty()) {
                appendLog(fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                    LogLevel::Error
                );
            }
            break;

        case openspace::HorizonsFile::ResultCode::ErrorSize: {
            std::string message = fmt::format("Time range '{}' to '{}' with step size "
                "'{} {}' is too big, try to increase the step size and/or decrease the "
                "time range", _startTime, _endTime, _stepEdit->text().toStdString(),
                _timeTypeCombo->currentText().toStdString());
            appendLog(message, HorizonsDialog::LogLevel::Error);
            styleLabel(_startLabel, true);
            styleLabel(_endLabel, true);
            styleLabel(_stepLabel, true);
            break;
        }

        case openspace::HorizonsFile::ResultCode::ErrorSpan:
            appendLog(
                "Step size is too big, exceeds available time span for target",
                HorizonsDialog::LogLevel::Error
            );
            styleLabel(_stepLabel, true);
            break;

        case openspace::HorizonsFile::ResultCode::ErrorTimeRange: {
            appendLog(
                fmt::format("Time range is outside the valid range for target '{}'",
                _targetName), HorizonsDialog::LogLevel::Error);
            styleLabel(_startLabel, true);
            styleLabel(_endLabel, true);

            _validTimeRange = readTimeRange();
            if (_validTimeRange.first.empty() || _validTimeRange.second.empty()) {
                appendLog(
                    "Could not parse the valid time range from file. "
                    "For more information, see the saved horizons file",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    appendLog(
                        fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                        LogLevel::Error
                    );
                }
                return false;
            }

            appendLog(
                fmt::format("Valid time range is '{}' to '{}'", _validTimeRange.first,
                _validTimeRange.second),
                HorizonsDialog::LogLevel::Info
            );
            _importTimeButton->show();
            break;
        }

        case openspace::HorizonsFile::ResultCode::ErrorNoObserver:
            appendLog(
                fmt::format("No match was found for observer '{}'", _observerName),
                HorizonsDialog::LogLevel::Error
            );
            appendLog(
                fmt::format("Try to use '@{}' as observer to search for possible "
                "matches.", _observerName),
                HorizonsDialog::LogLevel::Info
            );
            styleLabel(_centerLabel, true);
            break;

        case openspace::HorizonsFile::ResultCode::ErrorObserverTargetSame:
            appendLog(
                fmt::format("The observer '{}' and target '{}' are the same. Please use "
                "another observer for the current target.", _observerName, _targetName),
                HorizonsDialog::LogLevel::Error
            );
            styleLabel(_targetLabel, true);
            styleLabel(_centerLabel, true);
            break;

        case openspace::HorizonsFile::ResultCode::ErrorNoData:
            appendLog(
                fmt::format("There is not enough data to compute the state of target "
                "'{}' in relation to the observer '{}' for the time range '{}' to '{}'. "
                "Try to use another observer for the current target or another time "
                "range.", _targetName, _observerName, _startTime, _endTime),
                HorizonsDialog::LogLevel::Error
            );
            break;

        case openspace::HorizonsFile::ResultCode::MultipleObserverStations: {
            appendLog(
                fmt::format("Multiple matching observer stations were found for observer "
                "'{}'. ", _observerName), HorizonsDialog::LogLevel::Warning
            );
            appendLog(
                fmt::format("Did not find what you were looking for? Use '@{}' as "
                "observer to search for alternatives.", _observerName),
                HorizonsDialog::LogLevel::Info
            );
            styleLabel(_centerLabel, true);

            std::vector<std::string> matchingstations =
                _horizonsFile.parseMatches(
                    "Observatory Name",
                    "Multiple matching stations found"
                );
            if (matchingstations.empty()) {
                appendLog("Could not parse the matching stations. "
                    "For more information, see the saved horizons file",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    appendLog(
                        fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                        LogLevel::Error
                    );
                }
                return false;
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
            appendLog(
                fmt::format("Multiple matches were found for observer '{}'",
                _observerName),
                HorizonsDialog::LogLevel::Warning
            );
            styleLabel(_centerLabel, true);

            std::vector<std::string> matchingObservers =
                _horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingObservers.empty()) {
                appendLog("Could not parse the matching observers. "
                    "For more information, see the saved horizons file",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    appendLog(
                        fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                        LogLevel::Error
                    );
                }
                return false;
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
            appendLog(
                fmt::format("No match was found for target '{}'", _targetName),
                HorizonsDialog::LogLevel::Error
            );
            appendLog(
                fmt::format("Try to use '{}*' as target to search for possible matches.",
                _targetName),
                HorizonsDialog::LogLevel::Info
            );
            styleLabel(_targetLabel, true);
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

            appendLog(
                fmt::format("Multiple matches were found for target '{}'", _targetName),
                HorizonsDialog::LogLevel::Warning
            );
            styleLabel(_targetLabel, true);

            std::vector<std::string> matchingTargets =
                _horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingTargets.empty()) {
                appendLog("Could not parse the matching targets. "
                    "For more information, see the saved horizons file",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    appendLog(
                        fmt::format("Latest Horizons error: {}",_latestHorizonsError),
                        LogLevel::Error
                    );
                }
                return false;
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
            appendLog(
                "Unknown error. For more information, see the saved horizons file",
                LogLevel::Error
            );
            if (!_latestHorizonsError.empty()) {
                appendLog(fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                    LogLevel::Error
                );
            }
            _errorMsg->setText("An unknown error occured");
            return false;

        default:
            if (!_latestHorizonsError.empty()) {
                appendLog(fmt::format("Latest Horizons error: {}", _latestHorizonsError),
                    LogLevel::Error
                );
            }
            _errorMsg->setText("Invalid result type");
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
            LERROR(message);
            break;
        case HorizonsDialog::LogLevel::Warning:
            htmlText = "<font color=\"Yellow\">(W)  ";
            LWARNING(message);
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
