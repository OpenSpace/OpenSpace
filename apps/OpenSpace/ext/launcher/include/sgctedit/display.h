#ifndef DISPLAY_H
#define DISPLAY_H

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
    explicit Display();
    ~Display();

private slots:
    void toggleWindows();

private:
    void addWindowControl();
    void removeWindowControl();
    void initializeLayout(unsigned int windowIndex, WindowControl* winCtrl);
    unsigned int _monitorResolution[2] = {1920, 1080};
    QRect _monitorRes = {0, 0, _monitorResolution[0], _monitorResolution[1]};
    QRect _widgetDims = {0, 0, 400, 400};
    std::vector<WindowControl*> _windowControl;
    unsigned int _nWindows = 0;
    QPushButton* _toggleNumMonitorsButton = nullptr;
    MonitorBox* _monBox = nullptr;
    QVBoxLayout* _layout = nullptr;
    QHBoxLayout* _layoutMonButton = nullptr;
    QHBoxLayout* _layoutWindows = nullptr;
    QFrame* _borderFrame = nullptr;
};

#endif // DISPLAY_H
