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
#include "include/display.h"


Display::Display()
{
    _toggleNumMonitorsButton = new QPushButton("Add 2nd Window", this);
    _toggleNumMonitorsButton->setObjectName("toggleNumMonitors");

    //WindowControl wCtrl(0, _widgetDims, _monitorRes, this);
    _monBox = new MonitorBox(_widgetDims, _monitorRes, this);
    addWindowControl();
    QLineEdit* size_x = _windowControl.back()->lineEditSizeWidth();
    QLineEdit* size_y = _windowControl.back()->lineEditSizeHeight();
    QLineEdit* offset_x = _windowControl.back()->lineEditSizeOffsetX();
    QLineEdit* offset_y = _windowControl.back()->lineEditSizeOffsetY();

    _layout = new QVBoxLayout(this);
    _layout->addWidget(_monBox);
    _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    _monBox->setFixedSize(300, 300);
    _layout->addWidget(_toggleNumMonitorsButton);

    QLabel* label_size = new QLabel(this);
    QLabel* label_delim = new QLabel(this);
    QBoxLayout* sizeLayout = new QHBoxLayout(this);
    sizeLayout->addWidget(label_size);
    sizeLayout->addWidget(size_x);
    sizeLayout->addWidget(label_delim);
    sizeLayout->addWidget(size_y);
    _layout->addLayout(sizeLayout);

    QLabel* label_offset = new QLabel(this);
    QLabel* label_comma = new QLabel(this);
    QBoxLayout* offsetLayout = new QHBoxLayout(this);
    offsetLayout->addWidget(label_offset);
    offsetLayout->addWidget(offset_x);
    offsetLayout->addWidget(label_comma);
    offsetLayout->addWidget(offset_y);
    _layout->addLayout(offsetLayout);

    this->setLayout(_layout);
    label_size->setText("Size:");
    label_delim->setText(" x ");
    label_offset->setText("Offset:");
    label_comma->setText(" , ");

    int windowSize_x = std::stoi(size_x->text().toStdString());
    int windowSize_y = std::stoi(size_y->text().toStdString());
    QRect defaultMonitorResolution(_monitorResolution[0], _monitorResolution[1], 0, 0);
    _monBox->setResolution(defaultMonitorResolution);
}

Display::~Display() {
    delete _toggleNumMonitorsButton;
    delete _monBox;
    delete _layout;
}

void Display::windowResizedCallback(unsigned int windowIndex, const QRectF& newDims) {

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
        _windowControl.back()->setWindowChangeCallback(windowResizedCallback);
        _monBox->mapWindowResolutionToWidgetCoordinates(_nWindows,
            _windowControl.back()->dimensions());
        _nWindows++;
    }
}
