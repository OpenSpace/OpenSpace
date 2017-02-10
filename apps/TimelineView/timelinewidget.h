/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_APP_TIMELINEVIEW___TIMELINEWIDGET___H__
#define __OPENSPACE_APP_TIMELINEVIEW___TIMELINEWIDGET___H__

#include <QWidget>

#include "common.h"

#include <stdint.h>
#include <map>
#include <vector>

class QPaintEvent;

class TimelineWidget : public QWidget {
Q_OBJECT
public:
    TimelineWidget(QWidget* parent);

    void setData(std::vector<Image> images, std::map<uint8_t, std::string> targetMap, std::map<uint16_t, std::string> instrumentMap);
    void setCurrentTime(std::string currentTime, double et);
    void socketConnected();
    void socketDisconnected();

    std::string nextTarget() const;

protected:
    void paintEvent(QPaintEvent* event);  
    void drawContent(QPainter& painter, QRectF rect);
    void drawLegend(QPainter& painter, QRectF rect);
    void drawImages(QPainter& painter, QRectF timelineRect, std::vector<Image*> images, double minimumTime, double maximumTime);

private:
    std::vector<Image> _images;
    std::map<uint8_t, std::string> _targetMap;
    std::map<uint16_t, std::string> _instrumentMap;

    std::vector<std::string> _targets;
    std::vector<std::string> _instruments;

    struct {
        std::string time;
        double et;
    } _currentTime;
};

#endif // __OPENSPACE_APP_TIMELINEVIEW___TIMELINEWIDGET___H__
