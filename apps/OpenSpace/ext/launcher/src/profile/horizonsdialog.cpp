/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/boolean.h>
#include <QComboBox>
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QEventLoop>
#include <QFileDialog>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QPlainTextEdit>
#include <QProgressBar>
#include <QPushButton>
#include <QScrollBar>
#include <QStyle>
#include <sstream>

//using json = nlohmann::json;
//using namespace openspace;

namespace {
    constexpr std::string_view _loggerCat = "HorizonsDialog";

    // Text for the different units for the step size
    constexpr std::string_view TimeVarying = "arcseconds (time-varying)";
    constexpr std::string_view Minutes = "minutes";
    constexpr std::string_view Hours = "hours";
    constexpr std::string_view Days = "days";
    constexpr std::string_view Months = "calendar months";
    constexpr std::string_view Years = "calendar years";
    constexpr std::string_view Unitless = "equal intervals (unitless)";

    BooleanType(IsDirty);
    void styleLabel(QLabel* label, IsDirty isDirty) {
        const std::string newStyle = isDirty ? "error" : "normal";
        label->setObjectName(QString::fromStdString(newStyle));
        label->style()->unpolish(label);
        label->style()->polish(label);
    }

    int findId(const std::string& match) {
        // Format: id, other information...
        std::stringstream str(match);
        int id = 0;
        str >> id;
        return id;
    }
} // namespace

HorizonsDialog::HorizonsDialog(QWidget* parent)
    : QDialog(parent)
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    , _manager(new QNetworkAccessManager(this))
#endif // OPENSPACE_MODULE_SPACE_ENABLED
{

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    setWindowTitle("Horizons");
    createWidgets();
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

std::filesystem::path HorizonsDialog::file() const {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    return _horizonsFile.file();
#else // OPENSPACE_MODULE_SPACE_ENABLED
    return std::filesystem::path();
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

void HorizonsDialog::openSaveAs() {
    const QString filename = QFileDialog::getSaveFileName(
        this,
        "Choose a file path where the generated Horizons file will be saved",
        QString::fromStdString(absPath("${USER}").string()),
        "Horizons data file (*.hrz)",
        nullptr
#ifdef __linux__
        // Linux in Qt5 and Qt6 crashes when trying to access the native dialog here
        , QFileDialog::DontUseNativeDialog
#endif
    );
    _fileEdit->setText(filename);
}

void HorizonsDialog::typeOnChange(int index) {
    // Vector table type doesn't support time varying or arcseconds time steps
    if (index == 0 && _timeTypeCombo->itemText(0).toStdString() == TimeVarying) {
        _timeTypeCombo->removeItem(0);
    }
    // Observer
    else if (index == 1 && _timeTypeCombo->itemText(0).toStdString() != TimeVarying) {
        _timeTypeCombo->insertItem(0, TimeVarying.data());
    }
    else {
        _errorMsg->setText("Invalid Horizons type");
        styleLabel(_typeLabel, IsDirty::Yes);
    }
}

void HorizonsDialog::downloadProgress(int value, int total) {
    if (total < 0) {
        _downloadProgress->setRange(0, 0);
    }
    else {
        _downloadProgress->setRange(0, total);
        _downloadProgress->setValue(value);
    }
}

void HorizonsDialog::importTimeRange() {
    const QString startStr = QString::fromStdString(_validTimeRange.first);
    const QString endStr = QString::fromStdString(_validTimeRange.second);

    QDateTime start = QDateTime::fromString(startStr, "yyyy-MMM-dd T hh:mm");
    QDateTime end = QDateTime::fromString(endStr, "yyyy-MMM-dd T hh:mm");

    if (!start.isValid() || !end.isValid()) {
        start = QDateTime::fromString(startStr, "yyyy-MMM-dd T hh:mm:ss");
        end = QDateTime::fromString(endStr, "yyyy-MMM-dd T hh:mm:ss");

        if (!start.isValid() || !end.isValid()) {
            const QDate startDate = QDate::fromString(startStr, "yyyy-MMM-dd");
            const QDate endDate = QDate::fromString(endStr, "yyyy-MMM-dd");

            if (startDate.isValid() && endDate.isValid()) {
                _startEdit->setDate(startDate);
                _endEdit->setDate(endDate);
                _importTimeButton->hide();
                _validTimeRange = std::pair<std::string, std::string>();
                return;
            }

            _errorMsg->setText("Could not import time range");
            const std::string msg = std::format(
                "Could not import time range '{}' to '{}'",
                _validTimeRange.first, _validTimeRange.second
            );
            appendLog(msg, LogLevel::Error);
            return;
        }
    }
    _startEdit->setDateTime(start);
    _endEdit->setDateTime(end);
    _importTimeButton->hide();
    _validTimeRange = std::pair<std::string, std::string>();
}

// When the user presses the 'Save' button
void HorizonsDialog::approved() {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    _downloadLabel->show();
    const bool result = handleRequest();
    _downloadLabel->hide();
    if (result && std::filesystem::is_regular_file(_horizonsFile.file())) {
        accept();
    }
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

void HorizonsDialog::createWidgets() {
    QGridLayout* layout = new QGridLayout(this);
    layout->setSpacing(10);
    {
        QLabel* generateLabel = new QLabel("Generate a Horizons file", this);
        generateLabel->setObjectName("heading");
        layout->addWidget(generateLabel, 0, 0, 1, 3);

        QLabel* infoLabel = new QLabel(
            "<p>For more information about the Horizons system please visit: "
            "<a href=\"https://ssd.jpl.nasa.gov/horizons/\">"
            "https://ssd.jpl.nasa.gov/horizons/</a></p>"
        );
        infoLabel->setWordWrap(true);
        infoLabel->setObjectName("thin");
        infoLabel->setOpenExternalLinks(true);
        layout->addWidget(infoLabel, 1, 0, 1, 3);
    }
    {
        _typeLabel = new QLabel("Horizons data type:");
        _typeLabel->setToolTip("Choose Horizons data type");
        layout->addWidget(_typeLabel, 2, 0, 1, 2);

        _typeCombo = new QComboBox;
        _typeCombo->setToolTip("Choose Horizons data type");
        const QStringList types = {
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
        _fileLabel = new QLabel("File Path:");
        _fileLabel->setToolTip("Where the generated Horizons file is saved");
        layout->addWidget(_fileLabel, 3, 0);

        QBoxLayout* container = new QHBoxLayout;
        _fileEdit = new QLineEdit(this);
        _fileEdit->setToolTip("Where the generated Horizons file is saved");
        container->addWidget(_fileEdit);

        QPushButton* browseButton = new QPushButton("Browse");
        browseButton->setDefault(false);
        connect(
            browseButton, &QPushButton::released, this,
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
        _targetLabel = new QLabel("Target Body:");
        _targetLabel->setToolTip(
            "Which target or body would you like Horizons trajectery data for?"
        );
        layout->addWidget(_targetLabel, 4, 0);

        _targetEdit = new QLineEdit("Tesla");
        _targetEdit->setToolTip(
            "Which target or body would you like Horizons trajectery data for?"
        );
        layout->addWidget(_targetEdit, 4, 1, 1, 2);

        _chooseTargetCombo = new QComboBox;
        _chooseTargetCombo->setObjectName("mono");
        _chooseTargetCombo->hide();
        _chooseTargetCombo->setToolTip(
            "Choose a target from the search, or search again"
        );
        layout->addWidget(_chooseTargetCombo, 5, 1, 1, 2);
    }
    {
        _centerLabel = new QLabel("Observer Location:");
        _centerLabel->setToolTip("In which reference frame do you want the data in?");
        layout->addWidget(_centerLabel, 6, 0);

        _centerEdit = new QLineEdit(QString::fromStdString("@ssb"));
        _centerEdit->setToolTip("In which reference frame do you want the data in?");
        layout->addWidget(_centerEdit, 6, 1, 1, 2);

        _chooseObserverCombo = new QComboBox;
        _chooseObserverCombo->setObjectName("mono");
        _chooseObserverCombo->hide();
        _chooseObserverCombo->setToolTip(
            "Choose an observer from the search, or search again"
        );
        layout->addWidget(_chooseObserverCombo, 7, 1, 1, 2);
    }
    {
        _startLabel = new QLabel("Start Time:");
        _startLabel->setToolTip("Enter the start date and time for the data");
        layout->addWidget(_startLabel, 8, 0, 1, 2);
        _startEdit = new QDateTimeEdit;
        _startEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _startEdit->setDate(QDate::currentDate().addYears(-1));
        _startEdit->setToolTip("Enter the start date and time for the data");
        layout->addWidget(_startEdit, 8, 2);
    }
    {
        _endLabel = new QLabel("End Time:");
        _endLabel->setToolTip("Enter the end date and time for the data");
        layout->addWidget(_endLabel, 9, 0, 1, 2);
        _endEdit = new QDateTimeEdit(this);
        _endEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _endEdit->setDate(QDate::currentDate());
        _endEdit->setToolTip("Enter the end date and time for the data");
        layout->addWidget(_endEdit, 9, 2);
    }
    {
        _importTimeButton = new QPushButton("Import timerange");
        _importTimeButton->setDefault(false);
        connect(
            _importTimeButton, &QPushButton::released,
            this, &HorizonsDialog::importTimeRange
        );
        _importTimeButton->setCursor(Qt::PointingHandCursor);
        _importTimeButton->setToolTip("Import parsed timerange for full data coverage");
        _importTimeButton->hide();
        layout->addWidget(_importTimeButton, 10, 2);
    }
    {
        _stepLabel = new QLabel("Step Size:");
        _stepLabel->setToolTip("Enter the step size for the data");
        layout->addWidget(_stepLabel, 11, 0);

        _stepEdit = new QLineEdit;
        _stepEdit->setValidator(new QIntValidator(this));
        _stepEdit->setText(QString::number(1));
        _stepEdit->setToolTip("Enter the step size for the data");
        layout->addWidget(_stepEdit, 11, 1);

        _timeTypeCombo = new QComboBox;
        _timeTypeCombo->setToolTip("Choose unit of the step size");
        const QStringList timeTypes = {
            Minutes.data(),
            Hours.data(),
            Days.data(),
            Months.data(),
            Years.data(),
            Unitless.data()
        };
        _timeTypeCombo->addItems(timeTypes);
        _timeTypeCombo->setCurrentIndex(2);
        layout->addWidget(_timeTypeCombo, 11, 2);
    }
    layout->addWidget(new Line, 12, 0, 1, 3);
    {
        _log = new QPlainTextEdit;
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
        _downloadLabel = new QLabel("Downloading file...");
        _downloadLabel->setObjectName("thin");
        _downloadLabel->hide();
        layout->addWidget(_downloadLabel, 15, 0);

        _downloadProgress = new QProgressBar;
        _downloadProgress->hide();
        layout->addWidget(_downloadProgress, 15, 1, 1, 2);
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
        layout->addLayout(footer, 16, 0, 1, 3);
    }
}

bool HorizonsDialog::isValidInput() {
    // File
    if (_fileEdit->text().isEmpty()) {
        _errorMsg->setText("File path not selected");
        styleLabel(_fileLabel, IsDirty::Yes);
        return false;
    }

    // Target field
    if (_targetEdit->text().isEmpty() && ((_chooseTargetCombo->count() > 0 &&
        _chooseTargetCombo->currentIndex() == 0) || _chooseTargetCombo->count() == 0))
    {
        _errorMsg->setText("Target not selected");
        styleLabel(_targetLabel, IsDirty::Yes);
        return false;
    }
    if (_targetEdit->text().toStdString().find_first_of("¤<>§£´¨€") != std::string::npos)
    {
        _errorMsg->setText("Target includes illegal characters");
        styleLabel(_targetLabel, IsDirty::Yes);
        return false;
    }

    // Observer field
    if (_centerEdit->text().isEmpty() && ((_chooseObserverCombo->count() > 0 &&
        _chooseObserverCombo->currentIndex() == 0) || _chooseObserverCombo->count() == 0))
    {
        _errorMsg->setText("Observer not selected");
        styleLabel(_centerLabel, IsDirty::Yes);
        return false;
    }
    if (_centerEdit->text().toStdString().find_first_of("¤<>§£´¨€") != std::string::npos)
    {
        _errorMsg->setText("Observer includes illegal characters");
        styleLabel(_centerLabel, IsDirty::Yes);
        return false;
    }

    // Step size
    // Empty
    if (_stepEdit->text().isEmpty()) {
        _errorMsg->setText("Step size not selected");
        styleLabel(_stepLabel, IsDirty::Yes);
        return false;
    }
    // Numerical
    bool couldConvert = false;
    const int32_t step = _stepEdit->text().toInt(&couldConvert);
    if (!couldConvert) {
        _errorMsg->setText(QString::fromStdString(std::format(
            "Step size needs to be a number in range 1 to {}",
            std::numeric_limits<int32_t>::max()
        )));
        styleLabel(_stepLabel, IsDirty::Yes);
        return false;
    }
    // In the case of arcseconds range is different
    if (_timeTypeCombo->currentText().toStdString() == TimeVarying) {
        if (step < 60  || step > 3600) {
            _errorMsg->setText("Angular step size needs to be in range 60 to 3600");
            styleLabel(_stepLabel, IsDirty::Yes);
            return false;
        }
    }
    // Range 1 to 2147483647 (max of 32 bit int).
    // Horizons read the step size into a 32 bit int, but verifies the input on their
    // website as a uint32_t. If step size over 32 bit int is sent, this error message is
    // received: Cannot read numeric value -- re-enter
    if (step < 1) {
        _errorMsg->setText(QString::fromStdString(std::format(
            "Step size is outside valid range 1 to '{}'",
            std::numeric_limits<int32_t>::max()
        )));
        styleLabel(_stepLabel, IsDirty::Yes);
        return false;
    }
    return true;
}

// Send request synchronously, EventLoop waits until request has finished
nlohmann::json HorizonsDialog::sendRequest(const std::string& url) {
    QNetworkRequest request;
    request.setHeader(QNetworkRequest::UserAgentHeader, "OpenSpace");
    request.setUrl(QUrl(QString::fromStdString(url)));

    QNetworkReply* reply = _manager->get(request);
    connect(
        reply, &QNetworkReply::downloadProgress,
        this, &HorizonsDialog::downloadProgress
    );
    _downloadProgress->reset();
    _downloadProgress->show();

    QEventLoop loop;
    const QMetaObject::Connection status = connect(
        reply, &QNetworkReply::finished,
        &loop, &QEventLoop::quit
    );
    if (!status) {
        appendLog("Could not connect to Horizons API", HorizonsDialog::LogLevel::Error);
        return nlohmann::json();
    }

    loop.exec(QEventLoop::ExcludeUserInputEvents);

    return handleReply(reply);
}

nlohmann::json HorizonsDialog::handleReply(QNetworkReply* reply) {
    _downloadProgress->hide();
    if (reply->error() != QNetworkReply::NoError) {
        const QVariant statusCode =
            reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
        if (!checkHttpStatus(statusCode)) {
            const std::string msg = std::format(
                "Connection Error '{}'", reply->errorString().toStdString()
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);
        }

        reply->deleteLater();
        return nlohmann::json();
    }

    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (redirect.isValid() && reply->url() != redirect) {
        if (redirect.isRelative()) {
            redirect = reply->url().resolved(redirect);
        }

        const std::string msg = std::format(
            "Redirecting request to '{}'", redirect.toString().toStdString()
        );
        appendLog(msg, HorizonsDialog::LogLevel::Info);
        return sendRequest(redirect.toString().toStdString());
    }

    const QString answer = reply->readAll();
    reply->deleteLater();

    if (answer.isEmpty()) {
        const std::string msg = std::format(
            "Connection Error '{}'", reply->errorString().toStdString()
        );
        appendLog(msg, HorizonsDialog::LogLevel::Error);
        return nlohmann::json();
    }

    // Convert the answer to a json object and return it
    return nlohmann::json::parse(answer.toStdString());
}

bool HorizonsDialog::checkHttpStatus(const QVariant& statusCode) {
    bool isKnown = true;
    if (statusCode.isValid() &&
        statusCode.toInt() != static_cast<int>(HorizonsDialog::HTTPCodes::Ok))
    {
        std::string message;
        const HorizonsDialog::HTTPCodes code =
            static_cast<HorizonsDialog::HTTPCodes>(statusCode.toInt());

        switch (code) {
            case HorizonsDialog::HTTPCodes::BadRequest:
                message = "The request contained invalid keywords and/or used "
                    "a method other than GET or POST";
                break;
            case HorizonsDialog::HTTPCodes::MethodNotAllowed:
                message = "The request used an incorrect method";
                break;
            case HorizonsDialog::HTTPCodes::InternalServerError:
                message = "The database is currently not available, try again at a "
                    "later time";
                break;
            case HorizonsDialog::HTTPCodes::ServiceUnavailable:
                message = "The server is currently unable to handle the request due to a "
                    "temporary overloading or maintenance of the server, try again at a "
                    "later time";
                break;
            default:
                message = std::format(
                    "HTTP status code '{}' was returned",
                    statusCode.toString().toStdString()
                );
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

void HorizonsDialog::appendLog(const std::string& message, LogLevel level) {
    std::string htmlText;
    switch (level) {
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
    _log->appendHtml(QString::fromStdString(htmlText));
    _log->verticalScrollBar()->setValue(_log->verticalScrollBar()->maximum());
}

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
std::pair<std::string, std::string> HorizonsDialog::readTimeRange() {
    std::pair<std::string, std::string> timeRange = _horizonsFile.parseValidTimeRange(
        "Trajectory files",
        "************",
        "Trajectory name"
    );

    QDateTime start;
    QDateTime end;
    start = QDateTime::fromString(
        QString::fromStdString(timeRange.first),
        "yyyy-MMM-dd T hh:mm"
    );
    end = QDateTime::fromString(
        QString::fromStdString(timeRange.second),
        "yyyy-MMM-dd T hh:mm"
    );

    if (!start.isValid() || !end.isValid()) {
        start = QDateTime::fromString(
            QString::fromStdString(timeRange.first),
            "yyyy-MMM-dd T hh:mm:ss"
        );
        end = QDateTime::fromString(
            QString::fromStdString(timeRange.second),
            "yyyy-MMM-dd T hh:mm:ss"
        );

        if (!start.isValid() || !end.isValid()) {
            timeRange = _horizonsFile.parseValidTimeRange(
                "Trajectory files",
                "************",
                "Trajectory name",
                false
            );

            start = QDateTime::fromString(
                QString::fromStdString(timeRange.first),
                "yyyy-MMM-dd"
            );
            end = QDateTime::fromString(
                QString::fromStdString(timeRange.second),
                "yyyy-MMM-dd"
            );

            if (!start.isValid() || !end.isValid()) {
                if (timeRange.first.empty() || timeRange.second.empty()) {
                    _errorMsg->setText("Could not find time range");
                    appendLog(
                        "Could not find time range in Horizons file",
                        LogLevel::Error
                    );
                }
                else {
                    _errorMsg->setText("Could not parse time range");
                    const std::string msg = std::format(
                        "Could not read time range '{}' to '{}'",
                        timeRange.first, timeRange.second
                    );
                    appendLog(msg, LogLevel::Error);
                }
                return std::pair<std::string, std::string>();
            }
            else {
                const std::string msg = std::format(
                    "Could not find all time range information. Latest Horizons "
                    "mesage: {}", _latestHorizonsError
                );
                appendLog(msg, LogLevel::Warning);
            }
        }
    }
    return timeRange;
}

bool HorizonsDialog::handleRequest() {
    using namespace openspace;

    if (!isValidInput()) {
        return false;
    }

    // Reset
    _errorMsg->clear();

    //
    // Clean all widgets
    styleLabel(_typeLabel, IsDirty::No);
    styleLabel(_fileLabel, IsDirty::No);
    styleLabel(_targetLabel, IsDirty::No);
    styleLabel(_centerLabel, IsDirty::No);
    styleLabel(_startLabel, IsDirty::No);
    styleLabel(_endLabel, IsDirty::No);
    styleLabel(_stepLabel, IsDirty::No);

    _importTimeButton->hide();
    _validTimeRange = std::pair<std::string, std::string>();
    _latestHorizonsError.clear();

    const std::string url = constructUrl();

    _chooseObserverCombo->clear();
    _chooseObserverCombo->hide();
    _chooseTargetCombo->clear();
    _chooseTargetCombo->hide();

    nlohmann::json answer = sendRequest(url);
    if (answer.empty()) {
        _errorMsg->setText("Connection error");
        return false;
    }

    HorizonsFile file = handleAnswer(answer);
    if (!file.hasFile()) {
        return false;
    }

    _horizonsFile = std::move(file);
    HorizonsResultCode result = isValidHorizonsFile(_horizonsFile.file());

    const bool isValid = handleResult(result);
    if (!isValid && std::filesystem::is_regular_file(_horizonsFile.file())) {
        const std::string newName = _horizonsFile.file().filename().stem().string();

        const std::filesystem::path& oldFile = _horizonsFile.file();
        std::filesystem::path newFile = oldFile;
        newFile.replace_filename(newName + "_error.txt");

        std::filesystem::rename(oldFile, newFile);

        const std::string msg = std::format(
            "For more information, see the saved error file '{}'", newFile
        );
        appendLog(msg, LogLevel::Info);
    }

    return isValid;
}

std::string HorizonsDialog::constructUrl() {
    using namespace openspace;

    // Construct url for request
    HorizonsType type = HorizonsType::Invalid;
    if (_typeCombo->currentIndex() == 0) {
        type = HorizonsType::Vector;
    }
    else if (_typeCombo->currentIndex() == 1) {
        type = HorizonsType::Observer;
    }
    else {
        _errorMsg->setText("Invalid Horizons type");
        styleLabel(_typeLabel, IsDirty::Yes);
        return "";
    }

    std::string command;
    if (_chooseTargetCombo->count() > 0 && _chooseTargetCombo->currentIndex() != 0) {
        const QVariant t =
            _chooseTargetCombo->itemData(_chooseTargetCombo->currentIndex());
        command = t.toString().toStdString();
        _targetName = _chooseTargetCombo->currentText().toStdString();
        _targetEdit->setText(QString::fromStdString(command));
    }
    else {
        command = _targetEdit->text().toStdString();
        _targetName = command;
    }

    std::string center;
    if (_chooseObserverCombo->count() > 0 && _chooseObserverCombo->currentIndex() != 0) {
        const QVariant observer =
            _chooseObserverCombo->itemData(_chooseObserverCombo->currentIndex());
        const std::string id = observer.toString().toStdString();
        center = "@" + id;
        _observerName = _chooseObserverCombo->currentText().toStdString();
        _centerEdit->setText(QString::fromStdString(id));
    }
    else {
        center = _centerEdit->text().toStdString();
        _observerName = center;
    }

    _startTime = std::format(
        "{} {}",
        _startEdit->date().toString("yyyy-MM-dd").toStdString(),
        _startEdit->time().toString("hh:mm:ss").toStdString()
    );

    _endTime = std::format(
        "{} {}",
        _endEdit->date().toString("yyyy-MM-dd").toStdString(),
        _endEdit->time().toString("hh:mm:ss").toStdString()
    );

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
        styleLabel(_stepLabel, IsDirty::Yes);
        return "";
    }

    return constructHorizonsUrl(
        type,
        command,
        center,
        _startTime,
        _endTime,
        _stepEdit->text().toStdString(),
        unit
    );
}

openspace::HorizonsFile HorizonsDialog::handleAnswer(nlohmann::json& answer) {
    using namespace openspace;

    auto it = answer.find("error");
    if (it != answer.end()) {
        _latestHorizonsError = *it;
    }

    HorizonsResultCode isValid = isValidHorizonsAnswer(answer);
    if (isValid != HorizonsResultCode::Valid &&
        isValid != HorizonsResultCode::MultipleObserverStations &&
        isValid != HorizonsResultCode::ErrorTimeRange)
    {
        // Special case with MultipleObserverStations since it is detected as an error
        // but could be fixed by parsing the matches and let user choose
        // Special case with ErrorTimeRange since it is detected as an error
        // but could be nice to display the available time range of target to the user
        handleResult(isValid);
        return openspace::HorizonsFile();
    }

    // Create a text file and write reply to it
    const std::filesystem::path filePath =
        std::filesystem::absolute(_fileEdit->text().toStdString());

    auto result = answer.find("result");
    if (result == answer.end()) {
        const std::string msg = std::format(
            "Malformed answer received '{}'", answer.dump()
        );
        appendLog(msg, HorizonsDialog::LogLevel::Error);
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
        const int ret = msgBox.exec();
        switch (ret) {
            case QMessageBox::Yes:
                // If yes then continue as normal
                break;
            case QMessageBox::No:
            case QMessageBox::Cancel:
                _errorMsg->setText("File already exist, try another file path");
                styleLabel(_fileLabel, IsDirty::Yes);
                return openspace::HorizonsFile();
            default:
                _errorMsg->setText("Invalid answer");
                styleLabel(_fileLabel, IsDirty::Yes);
                return openspace::HorizonsFile();
        }
    }

    // Return a new file with the result
    return openspace::HorizonsFile(filePath, *result);
}

bool HorizonsDialog::handleResult(openspace::HorizonsResultCode& result) {
    using namespace openspace;

    switch (result) {
        case HorizonsResultCode::Valid: {
            // If the request worked then delete the corresponding error file if it exist
            std::filesystem::path validFile =_horizonsFile.file();

            const std::string errorName = validFile.filename().stem().string();
            const std::filesystem::path errorFile = validFile.replace_filename(
                errorName + "_error.txt"
            );

            if (std::filesystem::is_regular_file(errorFile)) {
                std::filesystem::remove(errorFile);
            }
            break;
        }
        case HorizonsResultCode::Empty: {
            _errorMsg->setText("The horizons file is empty");
            if (!_latestHorizonsError.empty()) {
                const std::string msg = std::format(
                    "Latest Horizons error: {}", _latestHorizonsError
                );
                appendLog(msg, LogLevel::Error);
            }

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorSize: {
            const std::string msg = std::format(
                "Time range '{}' to '{}' with step size '{} {}' is too big, try to "
                "increase the step size and/or decrease the time range",
                _startTime, _endTime, _stepEdit->text().toStdString(),
                _timeTypeCombo->currentText().toStdString()
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);
            styleLabel(_startLabel, IsDirty::Yes);
            styleLabel(_endLabel, IsDirty::Yes);
            styleLabel(_stepLabel, IsDirty::Yes);

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorSpan:
            appendLog(
                "Step size is too big, exceeds available time span for target",
                HorizonsDialog::LogLevel::Error
            );
            styleLabel(_stepLabel, IsDirty::Yes);

            std::filesystem::remove(_horizonsFile.file());
            break;
        case HorizonsResultCode::ErrorTimeRange: {
            std::string msg = std::format(
                "Time range is outside the valid range for target '{}'", _targetName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);
            styleLabel(_startLabel, IsDirty::Yes);
            styleLabel(_endLabel, IsDirty::Yes);

            _validTimeRange = readTimeRange();
            if (_validTimeRange.first.empty() || _validTimeRange.second.empty()) {
                if (!_latestHorizonsError.empty()) {
                    msg = std::format("Latest Horizons error: {}", _latestHorizonsError);
                    appendLog(msg, LogLevel::Error);
                }
                break;
            }

            msg = std::format(
                "Valid time range is '{}' to '{}'",
                _validTimeRange.first, _validTimeRange.second
            );
            appendLog(msg, HorizonsDialog::LogLevel::Info);
            _importTimeButton->show();

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorNoObserver: {
            std::string msg = std::format(
                "No match was found for observer '{}'", _observerName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);

            msg = std::format(
                "Try to use '@{}' as observer to search for possible matches",
                _observerName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Info);
            styleLabel(_centerLabel, IsDirty::Yes);

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorObserverTargetSame: {
            const std::string msg = std::format(
                "The observer '{}' and target '{}' are the same. Please use another "
                "observer for the current target", _observerName, _targetName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);
            styleLabel(_targetLabel, IsDirty::Yes);
            styleLabel(_centerLabel, IsDirty::Yes);

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorNoData: {
            const std::string msg = std::format(
                "There is not enough data to compute the state of target '{}' in "
                "relation to the observer '{}' for the time range '{}' to '{}'. Try to "
                "use another observer for the current target or another time range",
                _targetName, _observerName, _startTime, _endTime
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::MultipleObserverStations: {
            std::string msg = std::format(
                "Multiple matching observer stations were found for observer '{}'",
                _observerName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Warning);

            msg = std::format(
                "Did not find what you were looking for? Use '@{}' as observer to search "
                "for alternatives", _observerName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Info);
            styleLabel(_centerLabel, IsDirty::Yes);

            const std::vector<std::string> matchingstations =
                _horizonsFile.parseMatches(
                    "Observatory Name",
                    "Multiple matching stations found"
                );
            if (matchingstations.empty()) {
                appendLog(
                    "Could not parse the matching stations",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    msg = std::format("Latest Horizons error: {}", _latestHorizonsError);
                    appendLog(msg, LogLevel::Error);
                }
                break;
            }
            _chooseObserverCombo->clear();
            for (const std::string& station : matchingstations) {
                _chooseObserverCombo->addItem(
                    QString::fromStdString(station),
                    findId(station)
                );
            }
            _chooseObserverCombo->setCurrentIndex(0);
            _chooseObserverCombo->show();

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::MultipleObserver: {
            std::string msg = std::format(
                "Multiple matches were found for observer '{}'", _observerName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Warning);
            styleLabel(_centerLabel, IsDirty::Yes);

            const std::vector<std::string> matchingObservers =
                _horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingObservers.empty()) {
                appendLog(
                    "Could not parse the matching observers",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    msg = std::format("Latest Horizons error: {}", _latestHorizonsError);
                    appendLog(msg, LogLevel::Error);
                }
                break;
            }
            _chooseObserverCombo->clear();
            for (const std::string& observer : matchingObservers) {
                _chooseObserverCombo->addItem(
                    QString::fromStdString(observer),
                    findId(observer)
                );
            }
            _chooseObserverCombo->setCurrentIndex(0);
            _chooseObserverCombo->show();

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::ErrorNoTarget: {
            std::string msg = std::format(
                "No match was found for target '{}'", _targetName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Error);

            msg = std::format(
                "Try to use '{}*' as target to search for possible matches", _targetName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Info);
            styleLabel(_targetLabel, IsDirty::Yes);

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::MultipleTarget: {
            // Case Small Bodies:
            // Line before data: Matching small-bodies
            // Format: Record #, Epoch-yr, >MATCH DESIG<, Primary Desig, Name
            // Line after data:
            // (X matches. To SELECT, enter record # (integer), followed by semi-colon.)

            // Case Major Bodies:
            // Line before data: Multiple major-bodies match string "X*"
            // Format: ID#, Name, Designation, IAU/aliases/other
            // Line after data: Number of matches =  X. Use ID# to make unique selection.

            std::string msg = std::format(
                "Multiple matches were found for target '{}'", _targetName
            );
            appendLog(msg, HorizonsDialog::LogLevel::Warning);
            styleLabel(_targetLabel, IsDirty::Yes);

            const std::vector<std::string> matchingTargets =
                _horizonsFile.parseMatches("Name", "matches", ">MATCH NAME<");
            if (matchingTargets.empty()) {
                appendLog(
                    "Could not parse the matching targets",
                    HorizonsDialog::LogLevel::Error
                );
                if (!_latestHorizonsError.empty()) {
                    msg = std::format("Latest Horizons error: {}", _latestHorizonsError);
                    appendLog(msg, LogLevel::Error);
                }
                break;
            }
            _chooseTargetCombo->clear();
            for (const std::string& target : matchingTargets) {
                _chooseTargetCombo->addItem(
                    QString::fromStdString(target),
                    findId(target)
                );
            }
            _chooseTargetCombo->setCurrentIndex(0);
            _chooseTargetCombo->show();

            std::filesystem::remove(_horizonsFile.file());
            break;
        }
        case HorizonsResultCode::UnknownError: {
            appendLog("Unknown error", LogLevel::Error);
            if (!_latestHorizonsError.empty()) {
                const std::string msg = std::format(
                    "Latest Horizons error: {}", _latestHorizonsError
                );
                appendLog(msg, LogLevel::Error);
            }
            _errorMsg->setText("An unknown error occured");
            break;
        }
        default: {
            if (!_latestHorizonsError.empty()) {
                const std::string msg = std::format(
                    "Latest Horizons error: {}", _latestHorizonsError
                );
                appendLog(msg, LogLevel::Error);
            }
            _errorMsg->setText("Invalid result type");
            break;
        }
    }
    return result == HorizonsResultCode::Valid;
}
#endif // OPENSPACE_MODULE_SPACE_ENABLED
