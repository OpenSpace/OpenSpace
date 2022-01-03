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
    explicit Display(unsigned int monitorIdx, MonitorBox* monitorRenderBox,
         std::vector<QRect>& monitorSizeList, unsigned int numWindowsInit, bool showLabel);
    ~Display();

private slots:
    void addWindow();
    void removeWindow();

private:
    void initializeWindowControl();
    void removeWindowControl();
    void initializeLayout(bool showLabel, unsigned int numWindowsInit);
    void showWindows(unsigned int nWindowControlsDisplayed);
    void addDisplayLayout(unsigned int column, MonitorBox* monBox, QVBoxLayout* layout);
    std::vector<QRect>& _monitorResolutions;
    QRect _widgetDims = {0, 0, 400, 400};
    std::vector<WindowControl*> _windowControl;
    unsigned int _nWindowsAllocated = 0;
    unsigned int _nWindowsDisplayed = 0;
    QLabel* _labelMonNum = nullptr;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    unsigned int _monitorIdx = 0;
    MonitorBox* _monBox;
    QVBoxLayout* _layout = nullptr;
    QHBoxLayout* _layoutMonBox = nullptr;
    QHBoxLayout* _layoutMonNumLabel = nullptr;
    QHBoxLayout* _layoutMonButton = nullptr;
    QHBoxLayout* _layoutWindows = nullptr;
    std::vector<QVBoxLayout*> _winCtrlLayouts;
    std::vector<QWidget*> _layoutWindowWrappers;
    QFrame* _borderFrame = nullptr;
};

#endif // DISPLAY_H
