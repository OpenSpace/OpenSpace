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


struct ConfigResolution
{
    ConfigResolution(float width, float height) {
        _width = width;
        _height = height;
    }
    float _width;
    float _height;

    ConfigResolution& operator=(ConfigResolution& rhs) {
        _width = rhs._width;
        _height = rhs._height;
        return *this;
    }
};

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
    int numWindows();
    void windowDimensionsChanged(unsigned int index, const QRectF& newDimensions);
//    float monitorScaleFactor();
    void addWindowControl(WindowControl* wCtrl);

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    void redrawMonitor(ConfigResolution);
    void redrawWindow(ConfigResolution);

    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;

    QRect _monitorResolution;
    QRectF _monitorDimensionsScaled;
    std::vector<WindowControl*> _windowControl;

    std::vector<ConfigResolution> _windowResolutions;
    QRectF _monitorRendering;
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
