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

#include <QDialog>
#include <QNetworkReply>
#include <QSslConfiguration>
#include <string>

class QLabel;
class QDateTimeEdit;
class QNetworkReply;
class QSslConfiguration;

class HorizonsDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for HorizonsDialog class
     */
    HorizonsDialog(QWidget* parent);
    ~HorizonsDialog();

private slots:
    void openHorizonsFile();
    void sendHorizonsRequest();
    void handleReply(QNetworkReply *reply);
    void approved();

private:
    void createWidgets();
    void sendRequest(const std::string url);

    std::string _horizonsFile;
    QNetworkAccessManager* _manager;
    QSslConfiguration _config;

    QLabel* _startLabel;
    QDateTimeEdit* _startEdit;
    QLabel* _endLabel;
    QDateTimeEdit* _endEdit;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___HORIZONS___H__
