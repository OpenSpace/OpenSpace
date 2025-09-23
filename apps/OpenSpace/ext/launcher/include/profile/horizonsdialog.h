/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___HORIZONSDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___HORIZONSDIALOG___H__

#include <QDialog>

#include <openspace/json.h>
#include <filesystem>
#include <string>

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
#include <modules/space/horizonsfile.h>
#endif // OPENSPACE_MODULE_SPACE_ENABLED

class QComboBox;
class QDateTimeEdit;
class QLabel;
class QLineEdit;
class QNetworkAccessManager;
class QNetworkReply;
class QPlainTextEdit;
class QProgressBar;

class HorizonsDialog final : public QDialog {
Q_OBJECT
public:
    explicit HorizonsDialog(QWidget* parent);

    std::filesystem::path file() const;

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

    void openSaveAs();
    void typeOnChange(int index);
    void downloadProgress(int value, int total);
    void importTimeRange();

    void approved();

    bool isValidInput();
    nlohmann::json sendRequest(const std::string& url);
    nlohmann::json handleReply(QNetworkReply* reply);
    bool checkHttpStatus(const QVariant& statusCode);
    void appendLog(const std::string& message, LogLevel level);

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    std::pair<std::string, std::string> readTimeRange();
    bool handleRequest();
    std::string constructUrl();
    openspace::HorizonsFile handleAnswer(nlohmann::json& answer);
    bool handleResult(openspace::HorizonsResultCode& result);

    openspace::HorizonsFile _horizonsFile;
#endif // OPENSPACE_MODULE_SPACE_ENABLED

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

    std::string _latestHorizonsError;
};

#endif // __OPENSPACE_UI_LAUNCHER___HORIZONSDIALOG___H__
