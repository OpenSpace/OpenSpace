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
#include <../modules/space/horizonsfile.h>
#include <QDialog>
#include <filesystem>
#include <string>

class QComboBox;
class QLabel;
class QDateTimeEdit;
class QNetworkAccessManager;
class QNetworkReply;
class QLineEdit;
class QPlainTextEdit;

using json = nlohmann::json;

class HorizonsDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for HorizonsDialog class
     */
    HorizonsDialog(QWidget* parent);
    std::filesystem::path file() const;

private slots:
    void openDirectory();
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
    bool handleRequest();
    bool isValidInput();
    std::string constructUrl();
    json sendRequest(const std::string url);
    json handleReply(QNetworkReply* reply);
    bool checkHttpStatus(const QVariant& statusCode);
    std::filesystem::path handleAnswer(json& answer);
    bool handleResult(openspace::HorizonsFile::ResultCode& result);
    void appendLog(const std::string& message, const LogLevel level);

    openspace::HorizonsFile _horizonsFile;
    QNetworkAccessManager* _manager;

    QComboBox* _typeCombo = nullptr;
    QLineEdit* _directoryEdit = nullptr;
    QLineEdit* _nameEdit = nullptr;
    QLineEdit* _targetEdit = nullptr;
    QComboBox* _chooseTargetCombo = nullptr;
    std::string _targetName;
    QLineEdit* _centerEdit = nullptr;
    QComboBox* _chooseObserverCombo = nullptr;
    std::string _observerName;
    QDateTimeEdit* _startEdit = nullptr;
    std::string _startTime;
    QDateTimeEdit* _endEdit = nullptr;
    std::string _endTime;
    QLineEdit* _stepEdit = nullptr;
    QComboBox* _timeTypeCombo = nullptr;
    QLabel* _downloadLabel = nullptr;
    QPlainTextEdit* _log = nullptr;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___HORIZONS___H__
