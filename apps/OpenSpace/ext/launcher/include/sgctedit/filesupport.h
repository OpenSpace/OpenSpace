#ifndef FILE_SUPPORT_H
#define FILE_SUPPORT_H

#include <QFrame>
#include <QLabel>
#include <QLayout>
#include <QPushButton>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>
#include <sgctedit/display.h>
#include <sgctedit/orientation.h>
#include <sgct/config.h>


class FileSupport : public QWidget
{
    Q_OBJECT

public:
    explicit FileSupport(QVBoxLayout* parentLayout, std::vector<QRect>& monitorList,
        std::vector<Display*>& displays, Orientation* orientation,
        std::vector<sgct::config::Window>& windowList, sgct::config::Cluster& cluster,
        std::function<void(bool)> cb);
    ~FileSupport();
    bool isWindowFullscreen(sgct::ivec2 mDims, sgct::ivec2 wDims);
    int findGuiWindow();
    void saveCluster();
    void saveWindows();
    void saveProjectionInformation(bool isSpoutSelected, int projectionIndex,
        WindowControl* winControl, sgct::config::Viewport& viewport);
    std::string saveFilename();

private slots:
    void filenameEdited(const QString& newString);
    void cancel();
    void save();

private:
    QHBoxLayout* _layoutButtonBox = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    Orientation* _orientationWidget;
    std::vector<Display*>& _displayWidgets;
    std::vector<QRect>& _monitors;
    sgct::config::Cluster& _cluster;
    std::vector<sgct::config::Window>& _windowList;
    QLineEdit* _lineFilename = nullptr;
    std::function<void(bool)> _finishedCallback;
};

#endif // FILE_SUPPORT_H
