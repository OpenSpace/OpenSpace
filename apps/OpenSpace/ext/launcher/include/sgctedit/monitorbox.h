#ifndef MONITORBOX_H
#define MONITORBOX_H

#include <QColor>
#include <QIntValidator>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QVector>
#include <QWidget>

#include <algorithm>
#include <vector>
#include <iostream>

#include "windowcontrol.h"


class MonitorBox : public QWidget
{
Q_OBJECT
public:
    explicit MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution,
        unsigned int nWindows, bool showMonitorLabel, QString* winColors);
    ~MonitorBox();
    void mapMonitorResolutionToWidgetCoordinates();
    void mapWindowResolutionToWidgetCoordinates(unsigned int mIdx, unsigned int wIdx,
        const QRectF& w);
    void setNumWindowsDisplayed(unsigned int nWindows);
    void windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
            const QRectF& newDimensions);

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    void paintWidgetBorder(QPainter& painter, int width, int height);
    void paintMonitorBackgrounds(QPainter& painter);
    void paintWindow(QPainter& painter, unsigned int winIdx);
    void paintWindowBeyondBounds(QPainter& painter, unsigned int winIdx);
    void paintWindowNumber(QPainter& painter, unsigned int winIdx);
    void setPenSpecificToWindow(QPainter& painter, unsigned int windowIdx,
        bool visibleBorder);
    void computeScaledResolution_landscape(float aspectRatio, float maxWidth);
    void computeScaledResolution_portrait(float aspectRatio, float maxHeight);
    unsigned int _maxNumMonitors = 2;
    std::vector<QRect> _monitorResolution;
    //std::vector<QRectF> _monitorRendering;
    std::vector<QRectF> _monitorDimensionsScaled;

    std::vector<QRectF> _windowResolutions;
    std::vector<QRectF> _windowRendering = {
        {0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0}
    };
    QString* _colorsForWindows;
    int _alphaWindowOpacity = 170;
    float _monitorScaleFactor = 1.0;
    float _offset[2] = {10.0, 10.0};
    bool _showLabel = false;

    float _marginFractionOfWidgetSize = 0.025;
    float _marginWidget = 5.0;
    std::vector<QSizeF> _monitorOffsets;

    QRectF _monitorWidgetSize;
    QRectF _monitorBoundaryRect;


    unsigned int _nMonitors = 1;
    unsigned int _nWindows = 1;
};

#endif // MONITORBOX_H
