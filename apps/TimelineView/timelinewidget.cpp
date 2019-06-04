/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <QMap>
#include <QPainter>
#include <QPaintEvent>

#include <iostream>
#include <set>

namespace {
    static const int LegendHeight = 105;
    static const int TimeWidth = 200;

    static const int TextOffset = 5;

    QMap<QString, QColor> InstrumentColors = {
        { "NH_ALICE_AIRGLOW", QColor(40, 130, 200) },
        { "NH_ALICE_SOC", QColor(49, 234, 219) },
        { "NH_RALPH_LEISA", QColor(139, 86, 152) },
        { "NH_RALPH_MVIC_NIR", QColor(100, 14, 14) },
        { "NH_RALPH_MVIC_METHANE", QColor(211, 154, 31) },
        { "NH_RALPH_MVIC_RED", QColor(175, 18, 18) },
        { "NH_RALPH_MVIC_BLUE", QColor(84, 79, 149) },
        { "NH_LORRI", QColor(149, 219, 32) },
        { "NH_REX", QColor(35, 185, 125) },

        { "NH_RALPH_MVIC_PAN1", QColor(203, 153, 200) },
        { "NH_RALPH_MVIC_FT", QColor(242, 101, 74) },
        { "NH_RALPH_MVIC_PAN2", QColor(180, 180, 140) }
    };

    //{ "NH_ALICE_AIRGLOW", QColor(82, 145, 57) },
    //{ "NH_ALICE_SOC", QColor(241, 231, 48) },


    QMap<QString, QString> InstrumentConversion = {
        { "NH_ALICE_AIRGLOW", "ALICE Airglow" },
        { "NH_RALPH_LEISA", "RALPH LEISA" },
        { "NH_RALPH_MVIC_NIR", "RALPH MVIC NIR" },
        { "NH_ALICE_SOC", "ALICE SOC" },
        { "NH_RALPH_MVIC_BLUE", "RALPH MVIC Blue" },
        { "NH_RALPH_MVIC_PAN1" , "RALPH MVIC Pan1" },
        { "NH_LORRI", "LORRI" },
        { "NH_RALPH_MVIC_FT", "RALPH MVIC FT" },
        { "NH_RALPH_MVIC_PAN2", "RALPH MVIC Pan2" },
        { "NH_RALPH_MVIC_METHANE", "RALPH MVIC Methane" },
        { "NH_RALPH_MVIC_RED", "RALPH MVIC Red" },
        { "NH_REX", "REX" }
    };

    const double etSpread = 100.0;
}

TimelineWidget::TimelineWidget(QWidget* parent)
    : QWidget(parent)
    , _currentTime{"", 0.0}
{
    setMinimumWidth(600);
    setMinimumHeight(600);
}

void TimelineWidget::paintEvent(QPaintEvent* event) {
    QPainter painter(this);

    QRectF fullRect = contentsRect();
    QRectF contentRect(0, 0, fullRect.width() - 1, fullRect.height() - LegendHeight);
    QRectF legendRect(
        0,
        fullRect.bottom() - LegendHeight,
        fullRect.right(),
        fullRect.bottom()
    );

    painter.save();
    drawContent(painter, contentRect);
    painter.restore();

    painter.save();
    painter.translate(0, fullRect.height() - LegendHeight);
    drawLegend(painter, QRectF(legendRect));
    painter.restore();
}

void TimelineWidget::setData(std::vector<Image> images,
                             std::map<uint8_t, std::string> targetMap,
                             std::map<uint16_t, std::string> instrumentMap)
{
    _images.insert(_images.end(), images.begin(), images.end());

    std::sort(
        _images.begin(),
        _images.end(),
        [](const Image& a, const Image& b) { return a.beginning < b.beginning; }
    );

    _targetMap.insert(targetMap.begin(), targetMap.end());
    _instrumentMap.insert(instrumentMap.begin(), instrumentMap.end());

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
    //painter.setBrush(QBrush(Qt::lightGray));  painter.drawRect(timelineRect);
    painter.setBrush(QBrush(QColor(85, 85, 85)));  painter.drawRect(timelineRect);
    painter.setBrush(QBrush(QColor(165, 165, 165)));   painter.drawRect(dateRect);

    const double lowerTime = _currentTime.et - etSpread;
    const double upperTime = _currentTime.et + etSpread;

    std::vector<Image*> images;
    for (Image& i : _images) {
        if (i.beginning <= upperTime && i.ending >= lowerTime)
            images.push_back(&i);
    }

    drawImages(painter, timelineRect, images, lowerTime, upperTime);


    // Draw current time
    painter.setBrush(QBrush(Qt::black));
    painter.setPen(QPen(Qt::black, 2));
    painter.drawLine(
        QPointF(0, timelineRect.height() / 2),
        QPointF(timelineRect.width(), timelineRect.height() / 2)
    );
    painter.drawText(
        timelineRect.width(),
        timelineRect.height() / 2 + TextOffset,
        QString::fromStdString(_currentTime.time)
    );
}

void TimelineWidget::drawLegend(QPainter& painter, QRectF rect) {
    static const int Padding = 5;
    static const int BoxSize = 20;

    int currentHorizontalPosition = Padding;
    int currentVerticalPosition = Padding + BoxSize + Padding;

    // Draw Targets
    // Draw Instruments
    for (int i = 0; i < _instruments.size(); ++i) {
        if (i == _instruments.size() / 3 || i == _instruments.size() * 2 / 3) {
            currentVerticalPosition += BoxSize + Padding;
            currentHorizontalPosition = Padding;
        }

        const std::string& instrument = _instruments[i];
        ;
        painter.setBrush(QBrush(InstrumentColors[QString::fromStdString(instrument)]));
        painter.setPen(QPen(InstrumentColors[QString::fromStdString(instrument)]));
        painter.drawRect(
            currentHorizontalPosition,
            currentVerticalPosition,
            BoxSize,
            BoxSize
        );
        currentHorizontalPosition += BoxSize + Padding;

        painter.setPen(QPen(QColor(200, 200, 200)));
        //painter.setPen(QPen(Qt::black));
        painter.drawText(
            currentHorizontalPosition,
            currentVerticalPosition + BoxSize / 2 + TextOffset,
            InstrumentConversion[QString::fromStdString(instrument)]
        );
        currentHorizontalPosition += 125;
    }
}

void TimelineWidget::setCurrentTime(std::string currentTime, double et) {
    _currentTime.time = std::move(currentTime);
    _currentTime.et = std::move(et);
    repaint();

}

void TimelineWidget::drawImages(
    QPainter& painter,
    QRectF timelineRect,
    std::vector<Image*> images,
    double minimumTime, double maximumTime)
{
    std::set<std::string> instrumentSet;
    for (Image* i : images) {
        for (std::string instrument : i->instruments) {
            instrumentSet.insert(instrument);
        }
    }
    std::map<std::string, int> instruments;
    for (auto it = instrumentSet.begin(); it != instrumentSet.end(); ++it)
        instruments[*it] = std::distance(instrumentSet.begin(), it);

    for (Image* i : images) {
        double tBeg = (i->beginning - minimumTime) / (maximumTime - minimumTime);
        tBeg = std::max(tBeg, 0.0);
        double tEnd = (i->ending - minimumTime) / (maximumTime - minimumTime);
        tEnd = std::min(tEnd, 1.0);

        int loc = timelineRect.top() + timelineRect.height() * tBeg;
        int height = (timelineRect.top() + timelineRect.height() * tEnd) - loc;
        height = std::max(height, 5);

        if (loc + height > timelineRect.height()) {
            height = timelineRect.height() - loc;
        }

        std::string target = i->target;
        auto it = std::find(_targets.begin(), _targets.end(), target);
//        int iTarget = std::distance(_targets.begin(), it);

        for (std::string instrument : i->instruments) {
            auto it = std::find(_instruments.begin(), _instruments.end(), instrument);
            if (it == _instruments.end()) {
                qDebug() << "Instrument not found";
            }

            painter.setBrush(
                QBrush(InstrumentColors[QString::fromStdString(instrument)])
            );

            double width = timelineRect.width() / instruments.size();
            double pos = instruments[instrument] * width;

            painter.drawRect(pos, loc, width, height);
        }

        if (height >= 5) {
            painter.setBrush(QBrush(Qt::black));
            painter.setPen(QPen(Qt::black));
            QString line = QString::fromStdString(i->beginningString) + QString(" (") +
                           QString::fromStdString(i->target) + QString(")");

            painter.drawText(timelineRect.width(), loc + height / 2 + TextOffset, line);
        }
    }
}

void TimelineWidget::socketConnected() {
    setDisabled(false);
}

void TimelineWidget::socketDisconnected() {
    setDisabled(true);
    _images.clear();
    _instruments.clear();
    _targets.clear();
}

std::string TimelineWidget::nextTarget() const {
    auto it = std::lower_bound(
        _images.begin(),
        _images.end(),
        _currentTime.et,
        [](const Image& i, double et) { return i.beginning < et; }
    );
    if (it != _images.end()) {
        return it->target;
    }
    else {
        return "";
    }
}
