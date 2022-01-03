#include "display.h"
#include "orientation.h"
#include "orientationdialog.h"

Orientation::Orientation() {
    _orientationButton = new QPushButton("Global Orientation");
    _orientationButton->setObjectName("globalOrientation");
    _layoutOrientationButton = new QHBoxLayout();
    _layoutOrientationButton->addStretch(1);
    _layoutOrientationButton->addWidget(_orientationButton);
    _layoutOrientationButton->addStretch(1);
    connect(_orientationButton, SIGNAL(released()), this,
            SLOT(orientationDialog()));
}

void Orientation::addButtonToLayout(QVBoxLayout* parentLayout) {
    parentLayout->addLayout(_layoutOrientationButton);
}

void Orientation::orientationDialog() {
    OrientationDialog(this).exec();
}

Orientation::~Orientation()
{
    delete _layoutOrientationButton;
}

