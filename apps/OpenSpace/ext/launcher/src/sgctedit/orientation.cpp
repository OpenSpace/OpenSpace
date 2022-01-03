#include "display.h"
#include "orientation.h"
#include "orientationdialog.h"

Orientation::Orientation() {
    _layoutOrientationFull = new QHBoxLayout();
    _layoutOrientationControls = new QVBoxLayout();
    _orientationButton = new QPushButton("Global Orientation");
    _checkBoxVsync = new QCheckBox("VSync All Windows", this);
    _layoutOrientationControls->addWidget(_checkBoxVsync);
    _layoutOrientationControls->addWidget(_orientationButton);

    _layoutOrientationFull->addStretch(1);
    _layoutOrientationFull->addLayout(_layoutOrientationControls);
    _layoutOrientationFull->addStretch(1);

    connect(_orientationButton, SIGNAL(released()), this,
            SLOT(orientationDialog()));
}

void Orientation::addButtonToLayout(QVBoxLayout* parentLayout) {
    parentLayout->addLayout(_layoutOrientationFull);
}

void Orientation::orientationDialog() {
    OrientationDialog(this).exec();
}

Orientation::~Orientation()
{
    delete _orientationButton;
    delete _checkBoxVsync;
    delete _layoutOrientationFull;
    delete _layoutOrientationControls;
}

