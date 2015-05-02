/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "timelinewidget.h"

#include <QDebug>
#include <QPainter>
#include <QPaintEvent>

#include <set>

namespace {
    static const int LegendHeight = 105;
    static const int TimeWidth = 150;

    const QColor targetColors[] = {
        QColor(251, 180, 174),
        QColor(179, 205, 227),
        QColor(204, 235, 197),
        QColor(222, 203, 228),
        QColor(254, 217, 166),
        QColor(255, 255, 204)
    };

    const QColor instrumentColors[] = {
        QColor(228, 26, 28),
        QColor(55, 126, 184),
        QColor(77, 175, 74),
        QColor(152, 78, 163),
        QColor(255, 127, 0),
        QColor(255, 255, 51),
        QColor(166, 86, 40),
        QColor(247, 129, 191),
        QColor(153, 153, 153),
    };

    const double etSpread = 100.0;
}

TimelineWidget::TimelineWidget(QWidget* parent)
    : QWidget(parent)
{
    setMinimumWidth(600);
    setMinimumHeight(600);
}

void TimelineWidget::paintEvent(QPaintEvent* event) {
    QPainter painter(this);

    QRectF fullRect = contentsRect();
    QRectF contentRect(0, 0, fullRect.width() - 1, fullRect.height() - LegendHeight);
    QRectF legendRect(0, fullRect.bottom() - LegendHeight, fullRect.right(), fullRect.bottom());

    painter.save();
    drawContent(painter, contentRect);
    painter.restore();

    painter.save();
    painter.translate(0, fullRect.height() - LegendHeight);
    drawLegend(painter, QRectF(legendRect));
    painter.restore();
}

void TimelineWidget::setData(std::vector<Image> images, std::map<uint8_t, std::string> targetMap, std::map<uint16_t, std::string> instrumentMap) {
    _images = std::move(images);

    std::sort(_images.begin(), _images.end(), [](const Image& a, const Image& b) { return a.beginning < b.beginning; });

    _targetMap = std::move(targetMap);
    _instrumentMap = std::move(instrumentMap);

    _instruments.clear();
    std::set<std::string> instruments;
    for (auto p : _instrumentMap)
        instruments.insert(p.second);
    std::copy(instruments.begin(), instruments.end(), std::back_inserter(_instruments));

    _targets.clear();
    std::set<std::string> targets;
    for (auto p : _targetMap)
        targets.insert(p.second);
    std::copy(targets.begin(), targets.end(), std::back_inserter(_targets));

    repaint();
}

void TimelineWidget::drawContent(QPainter& painter, QRectF rect) {
    QRectF timelineRect(0, 0, rect.width() - TimeWidth, rect.height());
    QRectF dateRect(rect.width() - TimeWidth, 0, TimeWidth, rect.height());
    
    // Draw background
    painter.setBrush(QBrush(Qt::white));  painter.drawRect(timelineRect);
    painter.setBrush(QBrush(Qt::gray));   painter.drawRect(dateRect);

    // Draw current time
    painter.setBrush(QBrush(Qt::black));
    painter.drawLine(QPointF(0, timelineRect.height() / 2), QPointF(timelineRect.width(), timelineRect.height() / 2));
    painter.drawText(timelineRect.width(), timelineRect.height() / 2, QString::fromStdString(_currentTime.time));


    const double lowerTime = _currentTime.et - etSpread;
    const double upperTime = _currentTime.et + etSpread;

    std::vector<Image>::const_iterator start = _images.end();
    std::vector<Image>::const_iterator end = _images.end();
    //for (std::vector<Image>::const_iterator it = _images.begin(); it != _images.end(); ++it) {
    //    if (it->beginning > lowerTime 
    //}

    //std::vector<Image>::const_iterator lower = std::lower_bound(_images.begin(), _images.end(), lowerTime, [lowerTime](const Image& i) { return i.beginning < lowerTime; });
    ////std::vector<Image>::const_iterator upper = std::lower_bound(_images.begin(), _iamges.end(), upperTime)



}

void TimelineWidget::drawLegend(QPainter& painter, QRectF rect) {
    static const int HalfHeight = LegendHeight / 2;
    static const int Padding = 5;
    static const int BoxSize = 20;

    // Draw Targets
    int currentHorizontalPosition = Padding;
    int currentVerticalPosition = Padding;
    for (int i = 0; i < _targets.size(); ++i) {

        const std::string& target = _targets[i];
        
        painter.setBrush(QBrush(targetColors[i]));
        painter.drawRect(currentHorizontalPosition, currentVerticalPosition, BoxSize, BoxSize);
        currentHorizontalPosition += BoxSize + Padding;

        painter.drawText(currentHorizontalPosition, currentVerticalPosition + BoxSize / 2, QString::fromStdString(target));
        int textWidth = painter.boundingRect(QRect(), QString::fromStdString(target)).width();
        currentHorizontalPosition += std::max(textWidth, 25) + Padding;
    }

    // Draw Instruments
    currentHorizontalPosition = Padding;
    currentVerticalPosition = Padding + BoxSize + Padding;
    for (int i = 0; i < _instruments.size(); ++i) {
        if (i == _instruments.size() / 3 || i == _instruments.size() * 2 / 3) {
            currentVerticalPosition += BoxSize + Padding;
            currentHorizontalPosition = Padding;
        }

        const std::string& instrument = _instruments[i];

        //painter.setBrush(QBrush(instrumentColors[i]));
        painter.setBrush(Qt::NoBrush);
        painter.setPen(QPen(instrumentColors[i]));
        painter.drawRect(currentHorizontalPosition, currentVerticalPosition, BoxSize, BoxSize);
        currentHorizontalPosition += BoxSize + Padding;

        painter.setPen(QPen(Qt::black));
        painter.drawText(currentHorizontalPosition, currentVerticalPosition + BoxSize / 2, QString::fromStdString(instrument));
        int textWidth = painter.boundingRect(QRect(), QString::fromStdString(instrument)).width();
        currentHorizontalPosition += std::max(textWidth, 25) + Padding;
    }
}

void TimelineWidget::setCurrentTime(std::string currentTime, double et) {
    _currentTime.time = std::move(currentTime);
    _currentTime.et = std::move(et);
    repaint();

}

