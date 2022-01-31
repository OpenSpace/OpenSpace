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

#ifndef __OPENSPACE_UI_LAUNCHER___DISPLAY___H__
#define __OPENSPACE_UI_LAUNCHER___DISPLAY___H__

#include <QCheckBox>
#include <QComboBox>
#include <QIntValidator>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QPushButton>
#include <QTextBrowser>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>

#include "windowcontrol.h"
#include "monitorbox.h"


class Display : public QWidget
{
Q_OBJECT
public:
    explicit Display(MonitorBox* monitorRenderBox, std::vector<QRect>& monitorSizeList,
        const unsigned int nMaxWindows, const QString* winColors);
    ~Display();
    std::vector<WindowControl*> windowControls();
    unsigned int nWindows() const;
    void uncheckWebGuiOptions();
    void setindowChangeCallback(std::function<void(int, int, const QRectF&)> cb);

private slots:
    void addWindow();
    void removeWindow();

private:
    void initializeWindowControl();
    void removeWindowControl();
    void initializeLayout();
    void showWindows();
    void addDisplayLayout(unsigned int column, MonitorBox* monBox, QVBoxLayout* layout);
    MonitorBox* _monBox;
    std::vector<QRect>& _monitorResolutions;
    QRect _widgetDims = {0, 0, 400, 400};
    unsigned int _nWindowsAllocated = 0;
    unsigned int _nWindowsDisplayed = 0;
    unsigned int _nMaxWindows = 3;
    unsigned int _nMonitors = 1;
    const QString* _winColors;
    std::vector<WindowControl*> _windowControl;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    unsigned int _monitorIdx = 0;
    QVBoxLayout* _layout = nullptr;
    std::vector<QVBoxLayout*> _winCtrlLayouts;
    std::vector<QWidget*> _layoutWindowWrappers;
    std::vector<QFrame*> _frameBorderLines;
    QFrame* _borderFrame = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___DISPLAY___H__
