#include "display.h"
#include "orientation.h"
#include "orientationdialog.h"

Orientation::Orientation(QVBoxLayout* parentLayout)
{
    _orientationButton = new QPushButton("Global Orientation");
    _orientationButton->setObjectName("globalOrientation");
    _layoutOrientationButton = new QHBoxLayout();
    _layoutOrientationButton->addStretch(1);
    _layoutOrientationButton->addWidget(_orientationButton);
    _layoutOrientationButton->addStretch(1);
    parentLayout->addLayout(_layoutOrientationButton);
    connect(_orientationButton, SIGNAL(released()), this,
            SLOT(orientationDialog()));
}

void Orientation::orientationDialog() {
    OrientationDialog(this).exec();
}

Orientation::~Orientation()
{
    delete _layoutOrientationButton;
}

