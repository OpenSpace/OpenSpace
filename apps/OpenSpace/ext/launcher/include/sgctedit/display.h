#ifndef DISPLAY_H
#define DISPLAY_H

#include <QIntValidator>
#include <QLayout>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QPushButton>
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
    explicit Display();
    ~Display();
    static void windowResizedCallback(unsigned int windowIndex, const QRectF& newDims);

private:
    void addWindowControl();
    unsigned int _monitorResolution[2] = {1920, 1080};
    QRect _monitorRes = {0, 0, _monitorResolution[0], _monitorResolution[1]};
    QRect _widgetDims = {0, 0, 400, 400};
    std::vector<WindowControl*> _windowControl;
    unsigned int _nWindows = 0;
    QPushButton* _toggleNumMonitorsButton = nullptr;
    MonitorBox* _monBox = nullptr;
    QBoxLayout* _layout = nullptr;
};

#endif // DISPLAY_H
