#include <QApplication>
#include <QMainWindow>
#include <QScreen>
#include <string>

#include "sgctedit/monitorbox.h"
#include "sgctedit/windowcontrol.h"
#include "sgctedit/display.h"


Display::Display(MonitorBox* monitorRenderBox, std::vector<QRect>& monitorSizeList,
               std::function<void(unsigned int)> webGuiCallback, unsigned int nMaxWindows,
                                                                        QString* winColors)
    : _monBox(monitorRenderBox)
    , _monitorResolutions(monitorSizeList)
    , _webGuiCheckCallback(webGuiCallback)
    , _nMaxWindows(nMaxWindows)
    , _winColors(winColors)
{
    _addWindowButton = new QPushButton("Add Window >", this);
    _removeWindowButton = new QPushButton("< Remove Window", this);

    _nMonitors = _monitorResolutions.size();

    //Add all window controls (some will be hidden from GUI initially)
    for (unsigned int i = 0; i < _nMaxWindows; ++i) {
        initializeWindowControl();
    }
    initializeLayout();

    connect(_addWindowButton, SIGNAL(released()), this,
            SLOT(addWindow()));
    connect(_removeWindowButton, SIGNAL(released()), this,
            SLOT(removeWindow()));
}

Display::~Display() {
    delete _addWindowButton;
    delete _removeWindowButton;
    delete _monBox;
    for (auto f : _frameBorderLines) {
        delete f;
    }
    for (auto w : _windowControl) {
        delete w;
    }
    delete _layoutMonBox;
    delete _layoutMonButton;
    for (auto w : _layoutWindowWrappers) {
        delete w;
    }
    delete _layoutWindows;
    delete _layout;
}

void Display::initializeLayout() {
    _layout = new QVBoxLayout(this);
    _layoutMonButton = new QHBoxLayout();
    _layoutMonButton->addStretch(1);
    _layoutMonButton->addWidget(_removeWindowButton);
    _layoutMonButton->addWidget(_addWindowButton);
    _layoutMonButton->addStretch(1);
    _layout->addLayout(_layoutMonButton);
    _layoutWindows = new QHBoxLayout();
    _layout->addStretch();

    for (unsigned int i = 0; i < _nMaxWindows; ++i) {
        _winCtrlLayouts.push_back(_windowControl[i]->initializeLayout(this));
        _layoutWindowWrappers.push_back(new QWidget());
        _layoutWindowWrappers.back()->setLayout(_winCtrlLayouts.back());
        _layoutWindows->addWidget(_layoutWindowWrappers.back());
        if (i < (_nMaxWindows - 1)) {
            _frameBorderLines.push_back(new QFrame());
            _frameBorderLines.back()->setFrameShape(QFrame::VLine);
            _layoutWindows->addWidget(_frameBorderLines.back());
        }
    }
    _nWindowsDisplayed = 1;
    showWindows();
    _layout->addLayout(_layoutWindows);
}

std::vector<WindowControl*> Display::windowControls() {
    return _windowControl;
}

unsigned int Display::nWindows() {
    return _nWindowsDisplayed;
}

void Display::addWindow() {
    if (_nWindowsDisplayed < _nMaxWindows) {
        _nWindowsDisplayed++;
        showWindows();
    }
}

void Display::removeWindow() {
    if (_nWindowsDisplayed > 1) {
        _nWindowsDisplayed--;
        showWindows();
    }
}

void Display::showWindows() {
    for (unsigned int i = 0; i < _layoutWindowWrappers.size(); ++i) {
        _layoutWindowWrappers[i]->setVisible(i < _nWindowsDisplayed);
    }
    for (unsigned int i = 0; i < _frameBorderLines.size(); ++i) {
        if (i < (_nWindowsDisplayed - 1)) {
            _frameBorderLines[i]->setVisible(true);
        }
        else {
            _frameBorderLines[i]->setVisible(false);
        }
    }
    _removeWindowButton->setEnabled(_nWindowsDisplayed > 1);
    _removeWindowButton->setVisible(_nWindowsDisplayed > 1);
    _addWindowButton->setEnabled(_nWindowsDisplayed != _nMaxWindows);
    _addWindowButton->setVisible(_nWindowsDisplayed != _nMaxWindows);
    for (auto w : _windowControl) {
        w->showWindowLabel(_nWindowsDisplayed > 1);
    }
    _monBox->setNumWindowsDisplayed(_nWindowsDisplayed);
}

void Display::initializeWindowControl() {
    if (_nWindowsAllocated < _nMaxWindows) {
        unsigned int monitorNumForThisWindow = (_nWindowsAllocated >= 3) ? 1 : 0;
        _windowControl.push_back(
            new WindowControl(
                _nMonitors,
                monitorNumForThisWindow,
                _nWindowsAllocated,
                _widgetDims,
                _monitorResolutions,
                _winColors,
                this
            )
        );
        _windowControl.back()->setWindowChangeCallback(
            [this](int monIndex, int winIndex, const QRectF& newDims) {
                _monBox->windowDimensionsChanged(monIndex, winIndex, newDims);
            }
        );
        _windowControl.back()->setWebGuiChangeCallback(
            [this](unsigned int winIndex) {
                for (unsigned int w = 0; w < _nMaxWindows; ++w) {
                    if (w != winIndex) {
                        _windowControl[w]->uncheckWebGuiOption();
                    }
                }
            }
        );
        _monBox->mapWindowResolutionToWidgetCoordinates(
            monitorNumForThisWindow,
            _nWindowsAllocated,
            _windowControl.back()->dimensions()
        );
        _nWindowsAllocated++;
    }
}

void Display::uncheckWebGuiOptions() {
    for (auto w : _windowControl) {
        w->uncheckWebGuiOption();
    }
}

