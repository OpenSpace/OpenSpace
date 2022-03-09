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

#ifndef __OPENSPACE_UI_LAUNCHER___HORIZONS___H__
#define __OPENSPACE_UI_LAUNCHER___HORIZONS___H__

#include <openspace/json.h>
#include <QDialog>
#include <filesystem>
#include <string>

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
#include <../modules/space/horizonsfile.h>
#endif // OPENSPACE_MODULE_SPACE_ENABLED

class QComboBox;
class QLabel;
class QDateTimeEdit;
class QNetworkAccessManager;
class QNetworkReply;
class QLineEdit;
class QPlainTextEdit;
class QProgressBar;

using json = nlohmann::json;

class HorizonsDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for HorizonsDialog class
     */
    HorizonsDialog(QWidget* parent);

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    std::filesystem::path file() const;

private slots:
    void openSaveAs();
    void typeOnChange(int index);
    void downloadProgress(qint64 ist, qint64 max);
    void importTimeRange();
    void approved();

private:
    enum class LogLevel {
        Error,
        Warning,
        Info,
        None
    };

    enum class HTTPCodes {
        Ok = 200,
        BadRequest = 400,
        MethodNotAllowed = 405,
        InternalServerError = 500,
        ServiceUnavailable = 503
    };

    void createWidgets();
    void cleanAllWidgets();
    void styleLabel(QLabel* label, bool isDirty);
    bool handleRequest();
    bool isValidInput();
    std::string constructUrl();
    json sendRequest(const std::string url);
    json handleReply(QNetworkReply* reply);
    bool checkHttpStatus(const QVariant& statusCode);
    openspace::HorizonsFile handleAnswer(json& answer);
    bool handleResult(openspace::HorizonsFile::ResultCode& result);
    void appendLog(const std::string& message, const LogLevel level);

    openspace::HorizonsFile _horizonsFile;
    QNetworkAccessManager* _manager;

    QLabel* _typeLabel = nullptr;
    QComboBox* _typeCombo = nullptr;
    QLabel* _fileLabel = nullptr;
    QLineEdit* _fileEdit = nullptr;
    QLabel* _targetLabel = nullptr;
    QLineEdit* _targetEdit = nullptr;
    QComboBox* _chooseTargetCombo = nullptr;
    std::string _targetName;
    QLabel* _centerLabel = nullptr;
    QLineEdit* _centerEdit = nullptr;
    QComboBox* _chooseObserverCombo = nullptr;
    std::string _observerName;
    QLabel* _startLabel = nullptr;
    QDateTimeEdit* _startEdit = nullptr;
    std::string _startTime;
    QLabel* _endLabel = nullptr;
    QDateTimeEdit* _endEdit = nullptr;
    std::string _endTime;
    std::pair<std::string, std::string> _validTimeRange;
    QPushButton* _importTimeButton = nullptr;
    QLabel* _stepLabel = nullptr;
    QLineEdit* _stepEdit = nullptr;
    QComboBox* _timeTypeCombo = nullptr;
    QProgressBar* _downloadProgress = nullptr;
    QLabel* _downloadLabel = nullptr;
    QPlainTextEdit* _log = nullptr;

    QLabel* _errorMsg = nullptr;
    std::string _latestHorizonsError;
#endif // OPENSPACE_MODULE_SPACE_ENABLED
};

#endif // __OPENSPACE_UI_LAUNCHER___HORIZONS___H__
