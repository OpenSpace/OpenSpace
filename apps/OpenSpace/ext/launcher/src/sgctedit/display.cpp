#include <QApplication>
#include <QMainWindow>
#include <string>

#include "include/monitorbox.h"
#include "include/windowcontrol.h"
#include "include/display.h"


Display::Display()
{
    _toggleNumMonitorsButton = new QPushButton("Add 2nd Window", this);
    _toggleNumMonitorsButton->setObjectName("toggleNumMonitors");

    //WindowControl wCtrl(0, _widgetDims, _monitorRes, this);
    _monBox = new MonitorBox(_widgetDims, _monitorRes, this);
    addWindowControl();
    initializeLayout(0, _windowControl[0]);

    connect(_toggleNumMonitorsButton, SIGNAL(released()), this,
            SLOT(toggleWindows()));
}

Display::~Display() {
    delete _toggleNumMonitorsButton;
    delete _monBox;
    delete _layoutMonButton;
    delete _layoutWindows;
    delete _layout;
}

void Display::initializeLayout(unsigned int windowIndex, WindowControl* winCtrl) {
    _layout = new QVBoxLayout(this);
    _layout->addWidget(_monBox);
    _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    _monBox->setFixedSize(400, 400);
    _layoutMonButton = new QHBoxLayout(this);
    _layoutMonButton->addStretch(1);
    _layoutMonButton->addWidget(_toggleNumMonitorsButton);
    _layoutMonButton->addStretch(1);
    _layout->addLayout(_layoutMonButton);
    _layoutWindows = new QHBoxLayout(this);

    winCtrl->initializeLayout(this, _layoutWindows);
    _layout->addLayout(_layoutWindows);

    this->setLayout(_layout);

    QRect defaultMonitorResolution(_monitorResolution[0], _monitorResolution[1], 0, 0);
    _monBox->setResolution(defaultMonitorResolution);
}

void Display::toggleWindows() {
    if (_nWindows == 1) {
        addWindowControl();
        _borderFrame = new QFrame;
        _borderFrame->setFrameShape(QFrame::VLine);
        _layoutWindows->addWidget(_borderFrame);
        _windowControl.back()->initializeLayout(this, _layoutWindows);
        _toggleNumMonitorsButton->setText("Remove 2nd window");
    }
    else if (_nWindows == 2) {
        removeWindowControl();
        delete _borderFrame;
        _toggleNumMonitorsButton->setText("Add 2nd window");
    }
}

void Display::addWindowControl() {
    if (_nWindows < 2) {
        _windowControl.push_back(
            new WindowControl(
                _nWindows,
                _widgetDims,
                _monitorRes,
                this
            )
        );
        _windowControl.back()->setWindowChangeCallback(
            [this](unsigned int windowIndex, const QRectF& newDims) {
                _monBox->windowDimensionsChanged(windowIndex, newDims);
            }
        );
        _monBox->mapWindowResolutionToWidgetCoordinates(_nWindows,
            _windowControl.back()->dimensions());
        _nWindows++;
    }
}

void Display::removeWindowControl() {
    if (_nWindows == 2) {
        delete _windowControl.back();
        _windowControl.pop_back();
        _monBox->removeAdditionalWindowDimensions();
        _nWindows--;
    }
}
