#include <QApplication>
#include <QMainWindow>
#include <QScreen>
#include <string>

#include "include/monitorbox.h"
#include "include/windowcontrol.h"
#include "include/display.h"


Display::Display(unsigned int monitorIdx, MonitorBox* monitorRenderBox,
                                              unsigned int numWindowsInit, bool showLabel)
    : _monitorIdx(monitorIdx)
    , _monBox(monitorRenderBox)
{
    _addWindowButton = new QPushButton("Add Window", this);
    _removeWindowButton = new QPushButton("Remove Window", this);

    //Add 2 window controls
    addWindowControl();
    addWindowControl();
    initializeLayout(showLabel, numWindowsInit);

    connect(_addWindowButton, SIGNAL(released()), this,
            SLOT(addWindow()));
    connect(_removeWindowButton, SIGNAL(released()), this,
            SLOT(removeWindow()));
}

Display::~Display() {
    delete _addWindowButton;
    delete _monBox;
    delete _layoutMonBox;
    delete _layoutMonButton;
    delete _layoutWindows;
    delete _layout;
}

void Display::initializeLayout(bool showLabel, unsigned int numWindowsInit) {
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
    _layoutMonButton->addWidget(_removeWindowButton);
    _layoutMonButton->addWidget(_addWindowButton);
    _layoutMonButton->addStretch(1);
    _layout->addLayout(_layoutMonButton);
    _layoutWindows = new QHBoxLayout();

    _layout->addStretch();

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
    showWindows(numWindowsInit);
    _layout->addLayout(_layoutWindows);

    //for (WindowControl* w : _windowControl) {
    //    w->cleanupLayouts();
    //}
}

void Display::addWindow() {
    if (_nWindowsDisplayed == 0) {
        showWindows(1);
        _removeWindowButton->setEnabled(true);
    }
    else if (_nWindowsDisplayed == 1) {
        showWindows(2);
        _addWindowButton->setEnabled(false);
    }
}

void Display::removeWindow() {
    if (_nWindowsDisplayed == 1) {
        showWindows(0);
        _removeWindowButton->setEnabled(false);
    }
    else if (_nWindowsDisplayed == 2) {
        showWindows(1);
        _addWindowButton->setEnabled(true);
    }
}

void Display::showWindows(unsigned int nWindowControlsDisplayed) {
    _nWindowsDisplayed = nWindowControlsDisplayed;
    _borderFrame->setVisible(_nWindowsDisplayed == 2);
    _layoutWindowWrappers[0]->setVisible(_nWindowsDisplayed > 0);
    _layoutWindowWrappers[1]->setVisible(_nWindowsDisplayed == 2);
    _addWindowButton->setEnabled(_nWindowsDisplayed < 2);
    _removeWindowButton->setEnabled(_nWindowsDisplayed > 0);
    _monBox->setNumWindowsDisplayed(_monitorIdx, _nWindowsDisplayed);
    for (auto w : _windowControl) {
        w->showWindowLabel(_nWindowsDisplayed == 2);
    }
    if (_nWindowsDisplayed == 0) {
        _addWindowButton->setText("Add Window");
        _addWindowButton->setVisible(true);
        _removeWindowButton->setVisible(false);
    }
    else if (_nWindowsDisplayed == 1) {
        _addWindowButton->setText("Add 2nd Window");
        _removeWindowButton->setText("Remove Window");
        _addWindowButton->setVisible(true);
        _removeWindowButton->setVisible(true);
    }
    else if (_nWindowsDisplayed == 2) {
        _removeWindowButton->setText("Remove Window 2");
        _addWindowButton->setVisible(false);
        _removeWindowButton->setVisible(true);
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
