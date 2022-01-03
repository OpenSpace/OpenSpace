#include <QApplication>
#include <QLabel>
#include <QLayout>
#include <QWidget>
#include <QTextBrowser>
#include <QLineEdit>
#include <QComboBox>
#include <QPushButton>
#include <QMainWindow>
#include <string>

#include "include/monitorbox.h"
#include "include/windowcontrol.h"

void windowResizedCallback(unsigned int windowIndex, const QRectF& newDims) {

}

int main(int argc, char *argv[ ])
{
    QApplication app(argc, argv);
    QMainWindow win(nullptr);
    QWidget* centralWidget = new QWidget;
    win.setCentralWidget(centralWidget);

    unsigned int monitorResolution[2] = {1920, 1080};
    QPushButton* toggleNumMonitorsButton
        = new QPushButton("Add 2nd Window", centralWidget);
    toggleNumMonitorsButton->setObjectName("toggleNumMonitors");

    QLabel* label_size = new QLabel(centralWidget);
    QLabel* label_delim = new QLabel(centralWidget);

    QRect widgetDims(400, 400, 0, 0);
    QRect monitorRes(monitorResolution[0], monitorResolution[1], 0, 0);
    WindowControl wCtrl(0, widgetDims, monitorRes, centralWidget);
    MonitorBox* monBox = new MonitorBox(widgetDims, monitorRes, centralWidget);
    monBox->addWindowControl(&wCtrl);
    QLineEdit* size_x = wCtrl.lineEditSizeWidth();
    QLineEdit* size_y = wCtrl.lineEditSizeHeight();

    QBoxLayout* layout = new QVBoxLayout(centralWidget);
    layout->addWidget(monBox);
    layout->addWidget(toggleNumMonitorsButton);
    QBoxLayout* sizeLayout = new QHBoxLayout(centralWidget);
    sizeLayout->addWidget(label_size);
    sizeLayout->addWidget(size_x);
    sizeLayout->addWidget(label_delim);
    sizeLayout->addWidget(size_y);
    layout->addLayout(sizeLayout);

    label_size->setText("Size:");
    label_delim->setText(" x ");

    win.setWindowTitle("Test Qt14");

    int windowSize_x = std::stoi(size_x->text().toStdString());
    int windowSize_y = std::stoi(size_y->text().toStdString());
    QRect defaultMonitorResolution(monitorResolution[0], monitorResolution[1], 0, 0);
    monBox->setResolution(defaultMonitorResolution);

    win.show();
    app.exec();

    delete centralWidget;
    delete toggleNumMonitorsButton;
    delete monBox;
    delete layout;
}
