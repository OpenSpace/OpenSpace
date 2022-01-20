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
    explicit Display(MonitorBox* monitorRenderBox, std::vector<QRect>& monitorSizeList,
        std::function<void(unsigned int)> webGuiCallback, unsigned int nMaxWindows,
        QString* winColors);
    ~Display();
    std::vector<WindowControl*> windowControls();
    unsigned int nWindows();
    void uncheckWebGuiOptions();
    void setindowChangeCallback(std::function<void(int, int, const QRectF&)> cb);

private slots:
    void addWindow();
    void removeWindow();

private:
    void initializeWindowControl();
    void removeWindowControl();
    void initializeLayout();
    void showWindows();
    void addDisplayLayout(unsigned int column, MonitorBox* monBox, QVBoxLayout* layout);
    std::function<void(unsigned int)> _webGuiCheckCallback;
    std::vector<QRect>& _monitorResolutions;
    QRect _widgetDims = {0, 0, 400, 400};
    QString* _winColors;
    std::vector<WindowControl*> _windowControl;
    unsigned int _nWindowsAllocated = 0;
    unsigned int _nWindowsDisplayed = 0;
    unsigned int _nMaxWindows = 3;
    unsigned int _nMonitors = 1;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    unsigned int _monitorIdx = 0;
    MonitorBox* _monBox;
    QVBoxLayout* _layout = nullptr;
    QHBoxLayout* _layoutMonBox = nullptr;
    QHBoxLayout* _layoutMonButton = nullptr;
    QHBoxLayout* _layoutWindows = nullptr;
    std::vector<QVBoxLayout*> _winCtrlLayouts;
    std::vector<QWidget*> _layoutWindowWrappers;
    std::vector<QFrame*> _frameBorderLines;
    QFrame* _borderFrame = nullptr;
};

#endif // DISPLAY_H
