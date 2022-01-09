#ifndef __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__

#include <QApplication>
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
    void addDisplayLayout(unsigned int column, MonitorBox* monBox, QHBoxLayout* layout);
    void createWidgets();
    bool wasSaved() const;
    std::string saveFilename();

private:
    void systemMonitorConfiguration(QApplication& qtApp);
    MonitorBox* _monBox = nullptr;
    std::vector<QRect> _monitorSizeList;
    std::vector<QVBoxLayout*> _displayLayout = {nullptr, nullptr};
    std::vector<QFrame*> _displayFrame = {nullptr, nullptr};
    std::vector<Display*> _displayWidget = {nullptr, nullptr};
    QRect _monitorWidgetSize = {0, 0, 400, 350};
    FileSupport* _fileSupportWidget = nullptr;
    Orientation* _orientationWidget = nullptr;
    bool _showMonitorLabel = false;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    bool _saveSelected = false;
};

#endif // __OPENSPACE_UI_LAUNCHER___SGCTEDIT___H__
