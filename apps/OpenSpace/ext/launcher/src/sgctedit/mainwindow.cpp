#include "mainwindow.h"
#include "monitorbox.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    button_ = new QPushButton(tr("Push Me!"));
    textBrowser_ = new QTextBrowser();

    QGridLayout *mainLayout = new QGridLayout;
    mainLayout->addWidget(button_,0,0);
    mainLayout->addWidget(textBrowser_,1,0);
    setLayout(mainLayout);
    setWindowTitle(tr("Connecting buttons to processes.."));

/*    _layout = new QVBoxLayout();
    QBoxLayout* _monitorAreaLayout = new QHBoxLayout();

    QGridLayout* container = new QGridLayout;
    container->setColumnStretch(1, 1);

    _addSecondMonitor = new QPushButton("Add Monitor");
    container->addWidget(_addSecondMonitor);

    QLabel* dummyLabel = new QLabel("WTF?");
    container->addWidget(dummyLabel);

    _monitorBoxes[0] = new MonitorBox(this);
    _monitorBoxes[0]->setMonitorResolution({1920, 1080});
    _monitorBoxes[0]->setNumWindows(2);
    _monitorBoxes[0]->setWindowSize(0, {1000, 600}, {20, 10});
    _monitorBoxes[0]->setWindowSize(1, {400, 400}, {1020, 610});

    _monitorAreaLayout->addWidget(_monitorBoxes[0]);
    //_layout->addLayout(_monitorAreaLayout);
    _layout->addWidget(_addSecondMonitor);
    _layout->addWidget(dummyLabel);
    //_layout->addStretch(1);
    //_layout->addLayout(container);
    //_layout->addStretch(1);
*/
}

MainWindow::~MainWindow()
{
    delete button_;
    delete textBrowser_;
/*
    if (_monitorAreaLayout)
        delete _monitorAreaLayout;
    for (MonitorBox* mb : _monitorBoxes) {
        if (mb)
            delete mb;
    }
    if (_addSecondMonitor)
        delete _addSecondMonitor;
    if (_layout)
        delete _layout;
*/
}

