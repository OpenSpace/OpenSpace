#ifndef __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__

#include <QApplication>
#include <QColor>
#include <QDialog>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMainWindow>
#include <QComboBox>
#include <QPushButton>
#include <QScreen>
#include <QTextBrowser>
#include <QWidget>
#include <string>

#include <vector>
#include <iostream>

#include <sgctedit/display.h>
#include <sgctedit/filesupport.h>
#include <sgctedit/monitorbox.h>
#include <sgctedit/orientation.h>


class SgctEdit final : public QDialog
{
Q_OBJECT
public:
    SgctEdit(QWidget* parent, std::vector<sgct::config::Window>& windowList,
        sgct::config::Cluster& cluster, QApplication& qtApp);
    ~SgctEdit();
    void addDisplayLayout(MonitorBox* monBox, QHBoxLayout* layout);
    void createWidgets();
    bool wasSaved() const;
    std::string saveFilename();

private:
    void systemMonitorConfiguration(QApplication& qtApp);
    MonitorBox* _monBox = nullptr;
    std::vector<QRect> _monitorSizeList;
    QVBoxLayout* _displayLayout = nullptr;
    QFrame* _displayFrame = nullptr;
    Display* _displayWidget = nullptr;
    QRect _monitorWidgetSize = {0, 0, 500, 500};
    FileSupport* _fileSupportWidget = nullptr;
    Orientation* _orientationWidget = nullptr;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    bool _saveSelected = false;
    unsigned int _nMaxWindows = 3;
    QString _colorsForWindows[4] = {
        "#2B9EC3",
        "#FCAB10",
        "#44AF69",
        "#F8333C"
    };
};

#endif // __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
