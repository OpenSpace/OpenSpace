/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__

#include <QApplication>
#include <QColor>
#include <QDialog>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMainWindow>
#include <QComboBox>
#include <QPushButton>
#include <QScreen>
#include <QTextBrowser>
#include <QWidget>
#include <iostream>
#include <string>
#include <vector>
#include <sgctedit/display.h>
#include <sgctedit/filesupport.h>
#include <sgctedit/monitorbox.h>
#include <sgctedit/orientation.h>

class SgctEdit final : public QDialog
{
Q_OBJECT
public:
    SgctEdit(QWidget* parent, std::vector<sgct::config::Window>& windowList,
        sgct::config::Cluster& cluster, const QList<QScreen*> screenList);
    ~SgctEdit();
    void addDisplayLayout(MonitorBox* monBox, QHBoxLayout* layout);
    void createWidgets();
    bool wasSaved() const;
    std::string saveFilename();

private:
    void systemMonitorConfiguration(const QList<QScreen*> screenList);
    MonitorBox* _monBox = nullptr;
    std::vector<QRect> _monitorSizeList;
    QVBoxLayout* _displayLayout = nullptr;
    QFrame* _displayFrame = nullptr;
    Display* _displayWidget = nullptr;
    QRect _monitorWidgetSize = {0, 0, 500, 500};
    FileSupport* _fileSupportWidget = nullptr;
    Orientation* _orientationWidget = nullptr;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    bool _saveSelected = false;
    unsigned int _nMaxWindows = 3;
    QString _colorsForWindows[4] = {
        "#2B9EC3",
        "#FCAB10",
        "#44AF69",
        "#F8333C"
    };
};

#endif // __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
