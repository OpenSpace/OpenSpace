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


int main(int argc, char *argv[ ])
{
    QApplication app(argc, argv);
    QMainWindow win(nullptr);
    QWidget* centralWidget = new QWidget;
    win.setCentralWidget(centralWidget);

    QComboBox* monitorResolutionCombo = new QComboBox(centralWidget);
    monitorResolutionCombo->setObjectName("monitorResolution");
    QStringList monitorResolutionTypes = { "640x480", "1280x720", "1920x1080",
        "1920x1200", "2560x1440", "2048x1080", "3440x1440", "3840x2160", "7680x4320",
        "custom" };
    monitorResolutionCombo->addItems(monitorResolutionTypes);

    QPushButton* toggleNumMonitorsButton
        = new QPushButton("Add 2nd Window", centralWidget);
    toggleNumMonitorsButton->setObjectName("toggleNumMonitors");

    QLabel* label_size = new QLabel(centralWidget);
    QLabel* label_delim = new QLabel(centralWidget);
    QLineEdit* size_x = new QLineEdit("900", centralWidget);
    QLineEdit* size_y = new QLineEdit("500", centralWidget);
    QIntValidator* _validatorSize_x = new QIntValidator(10, 1920);
    QIntValidator* _validatorSize_y = new QIntValidator(10, 1080);
    size_x->setValidator(_validatorSize_x);
    size_y->setValidator(_validatorSize_y);
    MonitorBox* monBox = new MonitorBox(size_x, size_y, centralWidget);
    QBoxLayout* layout = new QVBoxLayout(centralWidget);
    layout->addWidget(monBox);
    layout->addWidget(toggleNumMonitorsButton);
    layout->addWidget(monitorResolutionCombo);
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
    monBox->setMonitorResolution({1920, 1080});
    monBox->setNumWindows(1);
    monBox->setWindowSize(0, {windowSize_x, 600}, {20, 10});
    monBox->setWindowSize(1, {400, 400}, {1020, 610});

    win.show();
    app.exec();

    delete centralWidget;
    delete monitorResolutionCombo;
    delete toggleNumMonitorsButton;
    delete monBox;
    delete layout;
}
