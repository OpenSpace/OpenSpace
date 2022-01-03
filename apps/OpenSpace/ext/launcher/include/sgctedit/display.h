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
    void initializeLayout();
    unsigned int _monitorResolution[2] = {1920, 1080};
    QRect _monitorRes = {0, 0, _monitorResolution[0], _monitorResolution[1]};
    QRect _widgetDims = {0, 0, 400, 400};
    std::vector<WindowControl*> _windowControl;
    unsigned int _nWindows = 0;
    int _lineEditWidthFixed = 80;
    QPushButton* _toggleNumMonitorsButton = nullptr;
    MonitorBox* _monBox = nullptr;
    QBoxLayout* _layout = nullptr;
    QBoxLayout* _layoutMonButton = nullptr;
    QLabel* _labelSize = nullptr;
    QLabel* _labelDelim = nullptr;
    QBoxLayout* _layoutSize = nullptr;
    QLineEdit* _lineSizeX = nullptr;
    QLineEdit* _lineSizeY = nullptr;
    QLineEdit* _lineOffsetX = nullptr;
    QLineEdit* _lineOffsetY = nullptr;
    QLabel* _labelOffset = nullptr;
    QLabel* _labelComma = nullptr;
    QBoxLayout* _layoutOffset = nullptr;
    QCheckBox* _optFullscreen = nullptr;
    QCheckBox* _optVsync = nullptr;
    QCheckBox* _optWebGui = nullptr;
    QCheckBox* _optSpoutOutput = nullptr;
    QBoxLayout* _layoutCBoxFullscreen = nullptr;
    QBoxLayout* _layoutCBoxVsync = nullptr;
    QBoxLayout* _layoutCBoxWebGui = nullptr;
    QBoxLayout* _layoutCBoxSpoutOutput = nullptr;
};

#endif // DISPLAY_H
