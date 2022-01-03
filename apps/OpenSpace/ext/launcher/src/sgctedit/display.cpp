#include <QApplication>
#include <QMainWindow>
#include <QScreen>
#include <string>

#include "include/monitorbox.h"
#include "include/windowcontrol.h"
#include "include/display.h"


Display::Display(unsigned int monitorIdx, MonitorBox* monitorRenderBox, bool showLabel)
    : _monitorIdx(monitorIdx)
    , _monBox(monitorRenderBox)
{
    _toggleNumWindowsButton = new QPushButton("Add 2nd Window", this);
    _toggleNumWindowsButton->setObjectName("toggleNumWindows");

    //Add 2 window controls
    addWindowControl();
    addWindowControl();
    initializeLayout(showLabel);

    connect(_toggleNumWindowsButton, SIGNAL(released()), this,
            SLOT(toggleWindows()));
}

Display::~Display() {
    delete _toggleNumWindowsButton;
    delete _monBox;
    delete _layoutMonBox;
    delete _layoutMonButton;
    delete _layoutWindows;
    delete _layout;
}

void Display::initializeLayout(bool showLabel) {
    _layout = new QVBoxLayout(this);

    if (showLabel) {
        _labelMonNum = new QLabel();
        _labelMonNum->setText("Display " + QString::number(_monitorIdx + 1));
        _layoutMonNumLabel = new QHBoxLayout();
        _layoutMonNumLabel->addStretch(1);
        _layoutMonNumLabel->addWidget(_labelMonNum);
        _layoutMonNumLabel->addStretch(1);
        _layout->addLayout(_layoutMonNumLabel);
    }

    _layoutMonButton = new QHBoxLayout();
    _layoutMonButton->addStretch(1);
    _layoutMonButton->addWidget(_toggleNumWindowsButton);
    _layoutMonButton->addStretch(1);
    _layout->addLayout(_layoutMonButton);
    _layoutWindows = new QHBoxLayout();

    _winCtrlLayouts.push_back(_windowControl[0]->initializeLayout(this));
    _layoutWindowWrappers.push_back(new QWidget());
    _layoutWindowWrappers.back()->setLayout(_winCtrlLayouts.back());
    _layoutWindows->addWidget(_layoutWindowWrappers.back());
    _borderFrame = new QFrame;
    _borderFrame->setFrameShape(QFrame::VLine);
    _layoutWindows->addWidget(_borderFrame);
    _winCtrlLayouts.push_back(_windowControl[1]->initializeLayout(this));
    _layoutWindowWrappers.push_back(new QWidget());
    _layoutWindowWrappers.back()->setLayout(_winCtrlLayouts.back());
    _layoutWindows->addWidget(_layoutWindowWrappers.back());
    hideSecondWindow();
    _layout->addLayout(_layoutWindows);

    //for (WindowControl* w : _windowControl) {
    //    w->cleanupLayouts();
    //}
}

void Display::toggleWindows() {
    if (_nWindowsDisplayed == 1) {
        _toggleNumWindowsButton->setText("Remove 2nd window");
        showSecondWindow();
    }
    else if (_nWindowsDisplayed == 2) {
        _toggleNumWindowsButton->setText("Add 2nd window");
        hideSecondWindow();
        int minWidth = minimumWidth();
    }
}

void Display::hideSecondWindow() {
    _borderFrame->setVisible(false);
    _layoutWindowWrappers[1]->setVisible(false);
    _nWindowsDisplayed = 1;
    _monBox->setNumWindowsDisplayed(_monitorIdx, _nWindowsDisplayed);
    for (auto w : _windowControl) {
        w->showWindowLabel(false);
    }
}

void Display::showSecondWindow() {
    _borderFrame->setVisible(true);
    _layoutWindowWrappers[1]->setVisible(true);
    _nWindowsDisplayed = 2;
    _monBox->setNumWindowsDisplayed(_monitorIdx, _nWindowsDisplayed);
    for (auto w : _windowControl) {
        w->showWindowLabel(true);
    }
}

void Display::addWindowControl() {
    if (_nWindowsAllocated < 2) {
        _windowControl.push_back(
            new WindowControl(
                _monitorIdx,
                _nWindowsAllocated,
                _widgetDims,
                this
            )
        );
        _windowControl.back()->setWindowChangeCallback(
            [this](unsigned int monIndex, unsigned int winIndex, const QRectF& newDims) {
                _monBox->windowDimensionsChanged(monIndex, winIndex, newDims);
            }
        );
        _monBox->mapWindowResolutionToWidgetCoordinates(
            _monitorIdx,
            _nWindowsAllocated,
            _windowControl.back()->dimensions()
        );
        _nWindowsAllocated++;
    }
}
