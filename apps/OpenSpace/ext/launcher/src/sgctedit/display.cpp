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
    initializeLayout();

    connect(_toggleNumMonitorsButton, SIGNAL(released()), this,
            SLOT(toggleWindows()));
}

Display::~Display() {
    delete _toggleNumMonitorsButton;
    delete _monBox;
    delete _layoutMonButton;
    delete _labelSize;
    delete _labelDelim;
    delete _layoutSize;
    delete _labelOffset;
    delete _labelComma;
    delete _layoutOffset;
    delete _optFullscreen;
    delete _optVsync;
    delete _optWebGui;
    delete _optSpoutOutput;
    delete _layoutCBoxFullscreen;
    delete _layoutCBoxVsync;
    delete _layoutCBoxWebGui;
    delete _layoutCBoxSpoutOutput;
}

void Display::initializeLayout() {
    _layout = new QVBoxLayout(this);
    _layout->addWidget(_monBox);
    _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    _monBox->setFixedSize(400, 400);
    _layoutMonButton = new QHBoxLayout(this);
    _layoutMonButton->addStretch(1);
    _layoutMonButton->addWidget(_toggleNumMonitorsButton);
    _layoutMonButton->addStretch(1);
    _layout->addLayout(_layoutMonButton);

    _layoutWindowCtrl = new QHBoxLayout(this);
    //////////Move the rest of the layout in this func to WindowControl class,
    // with _layoutWindowCtrl as parent layout

    //Window size
    _lineSizeX = _windowControl.back()->lineEditSizeWidth();
    _lineSizeY = _windowControl.back()->lineEditSizeHeight();
    _lineSizeX->setFixedWidth(_lineEditWidthFixed);
    _lineSizeY->setFixedWidth(_lineEditWidthFixed);
    _labelSize = new QLabel(this);
    _labelDelim = new QLabel(this);
    _layoutSize = new QHBoxLayout(this);
    _layoutSize->addStretch(1);
    _layoutSize->addWidget(_labelSize);
    _labelSize->setFixedWidth(60);
    _labelSize->setText("Size:");
    _layoutSize->addWidget(_lineSizeX);
    _layoutSize->addWidget(_labelDelim);
    _layoutSize->addWidget(_lineSizeY);
    _layoutSize->addStretch(1);
    _labelDelim->setText(" x ");
    _layout->addLayout(_layoutSize);

    //Window offset
    _lineOffsetX = _windowControl.back()->lineEditSizeOffsetX();
    _lineOffsetY = _windowControl.back()->lineEditSizeOffsetY();
    _lineOffsetX->setFixedWidth(_lineEditWidthFixed);
    _lineOffsetY->setFixedWidth(_lineEditWidthFixed);
    _labelOffset = new QLabel(this);
    _labelComma = new QLabel(this);
    _layoutOffset = new QHBoxLayout(this);
    _layoutOffset->addStretch(1);
    _layoutOffset->addWidget(_labelOffset);
    _labelOffset->setFixedWidth(60);
    _labelOffset->setText("Offset:");
    _layoutOffset->addWidget(_lineOffsetX);
    _layoutOffset->addWidget(_labelComma);
    _layoutOffset->addWidget(_lineOffsetY);
    _layoutOffset->addStretch(1);
    _labelComma->setText(" , ");
    _layout->addLayout(_layoutOffset);

    //Window options
    _optFullscreen = _windowControl.back()->checkBoxFullscreen();
    _layoutCBoxFullscreen = new QHBoxLayout(this);
    _layoutCBoxFullscreen->addStretch(1);
    _layoutCBoxFullscreen->addWidget(_optFullscreen);
    _layoutCBoxFullscreen->addStretch(1);
    _layout->addLayout(_layoutCBoxFullscreen);
    _optVsync = _windowControl.back()->checkBoxVsync();
    _layoutCBoxVsync= new QHBoxLayout(this);
    _layoutCBoxVsync->addStretch(1);
    _layoutCBoxVsync->addWidget(_optVsync);
    _layoutCBoxVsync->addStretch(1);
    _layout->addLayout(_layoutCBoxVsync);
    _optWebGui = _windowControl.back()->checkBoxWebGui();
    _layoutCBoxWebGui= new QHBoxLayout(this);
    _layoutCBoxWebGui->addStretch(1);
    _layoutCBoxWebGui->addWidget(_optWebGui);
    _layoutCBoxWebGui->addStretch(1);
    _layout->addLayout(_layoutCBoxWebGui);
    _optSpoutOutput = _windowControl.back()->checkBoxSpoutOutput();
    _layoutCBoxSpoutOutput= new QHBoxLayout(this);
    _layoutCBoxSpoutOutput->addStretch(1);
    _layoutCBoxSpoutOutput->addWidget(_optSpoutOutput);
    _layoutCBoxSpoutOutput->addStretch(1);
    _layout->addLayout(_layoutCBoxSpoutOutput);

    this->setLayout(_layout);

    int windowSize_x = std::stoi(_lineSizeX->text().toStdString());
    int windowSize_y = std::stoi(_lineSizeY->text().toStdString());
    QRect defaultMonitorResolution(_monitorResolution[0], _monitorResolution[1], 0, 0);
    _monBox->setResolution(defaultMonitorResolution);
}

void Display::toggleWindows() {
    std::cout << "Toggle." << std::endl;
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
