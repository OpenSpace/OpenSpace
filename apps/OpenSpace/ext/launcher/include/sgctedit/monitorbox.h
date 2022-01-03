#ifndef MONITORBOX_H
#define MONITORBOX_H

#include <QIntValidator>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>

#include "windowcontrol.h"


class MonitorBox : public QWidget
{
    Q_OBJECT

public:
    explicit MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution/*, QWidget *parent = nullptr*/);
    ~MonitorBox();
    void mapMonitorResolutionToWidgetCoordinates();
    void mapWindowResolutionToWidgetCoordinates(unsigned int mIdx, unsigned int wIdx,
        const QRectF& w);
    void setResolution(unsigned int index, QRect& res);
    void setNumWindowsDisplayed(unsigned int mIdx, unsigned int nWindows);
    int numWindows();
    void windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
            const QRectF& newDimensions);
    void addWindowControl(WindowControl* wCtrl);

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    unsigned int _maxNumMonitors = 2;
    unsigned int _maxNumWindowsPerMonitor = 2;
    std::vector<QRect> _monitorResolution;
    //std::vector<QRectF> _monitorRendering;
    std::vector<QRectF> _monitorDimensionsScaled;

    std::vector<QRectF> _windowResolutions;
    std::vector<std::vector<QRectF>> _windowRendering = {
        {
            {0.0, 0.0, 0.0, 0.0},
            {0.0, 0.0, 0.0, 0.0}
        },
        {
            {0.0, 0.0, 0.0, 0.0},
            {0.0, 0.0, 0.0, 0.0}
        }
    };
    float _monitorScaleFactor = 1.0;
    float _offset[2] = {10.0, 10.0};

    float _marginFractionOfWidgetSize = 0.025;
    float _marginWidget = 5.0;
    std::vector<QSizeF> _monitorOffsets;

    QRectF _monitorWidgetSize;
    QRectF _monitorBoundaryRect;


    unsigned int _nMonitors = 1;
    std::vector<unsigned int> _nWindows = {1, 0};
};

#endif // MONITORBOX_H
