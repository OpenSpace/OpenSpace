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
    explicit MonitorBox(QRect widgetDims, QRect monitorResolution,
        QWidget *parent = nullptr);
    ~MonitorBox();
    void mapMonitorResolutionToWidgetCoordinates(QRect r);
    void mapWindowResolutionToWidgetCoordinates(unsigned int index, const QRectF& w);
    void setResolution(QRect& res);
    void setNumWindowsDisplayed(unsigned int nWindows);
    int numWindows();
    void windowDimensionsChanged(unsigned int index, const QRectF newDimensions);
    void addWindowControl(WindowControl* wCtrl);
    void removeAdditionalWindowDimensions();

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;

    QRect _monitorResolution;
    QRectF _monitorDimensionsScaled;

    std::vector<QRectF> _windowResolutions;
    std::vector<QRectF*> _windowRendering;
    float _monitorScaleFactor = 1.0;
    float _offset[2] = {10.0, 10.0};

    float _marginFractionOfWidgetSize = 0.025;
    QRectF _monitorWidgetSize;
    QRectF _monitorBoundaryRect;
    float _marginWidget = 5.0;

    unsigned int _nWindows = 0;
};

#endif // MONITORBOX_H
