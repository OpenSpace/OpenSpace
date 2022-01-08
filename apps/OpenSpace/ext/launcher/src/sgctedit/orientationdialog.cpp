#include "sgctedit/display.h"
#include "sgctedit/orientationdialog.h"


OrientationDialog::OrientationDialog(sgct::quat& orientation, QWidget* parent)
    : _orientationValue(orientation)
    , QDialog(parent)
{
    setWindowTitle("Global Orientation");
    _layoutWindow = new QVBoxLayout(this);
    _layoutPitch = new QHBoxLayout();
    _layoutRoll = new QHBoxLayout();
    _layoutYaw = new QHBoxLayout();

    _labelPitch = new QLabel(this);
    _labelRoll = new QLabel(this);
    _labelYaw = new QLabel(this);
    _labelPitch->setText("Pitch: ");
    _labelRoll ->setText("Roll: ");
    _labelYaw ->setText("Yaw: ");

    _linePitch = new QLineEdit(this);
    _lineRoll = new QLineEdit(this);
    _lineYaw = new QLineEdit(this);
    _linePitch->setText(QString::number(_orientationValue.x));
    _lineRoll->setText(QString::number(_orientationValue.z));
    _lineYaw->setText(QString::number(_orientationValue.y));
     _validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
     _validatorPitch->setNotation(QDoubleValidator::StandardNotation);
     _validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
     _validatorRoll->setNotation(QDoubleValidator::StandardNotation);
     _validatorYaw = new QDoubleValidator(-180.0, 180.0, 15);
     _validatorYaw->setNotation(QDoubleValidator::StandardNotation);
     _linePitch->setValidator(_validatorPitch);
     _lineRoll->setValidator(_validatorRoll);
     _lineYaw->setValidator(_validatorYaw);

    _layoutPitch->addStretch(1);
    _layoutPitch->addWidget(_labelPitch);
    _layoutPitch->addWidget(_linePitch);
    //_layoutPitch->addStretch(1);
    _layoutWindow->addLayout(_layoutPitch);

    _layoutRoll->addStretch(1);
    _layoutRoll->addWidget(_labelRoll);
    _layoutRoll->addWidget(_lineRoll);
    //_layoutRoll->addStretch(1);
    _layoutWindow->addLayout(_layoutRoll);

    _layoutYaw->addStretch(1);
    _layoutYaw->addWidget(_labelYaw);
    _layoutYaw->addWidget(_lineYaw);
    //_layoutYaw->addStretch(1);
    _layoutWindow->addLayout(_layoutYaw);

    _layoutButtonBox = new QHBoxLayout;
    _buttonSave = new QPushButton("OK");
    _buttonSave->setToolTip("Save global orientation changes");
    //connect(_buttonSave, &QPushButton::clicked, this, &ModulesDialog::listItemSave);
    _layoutButtonBox->addStretch(1);
    _layoutButtonBox->addWidget(_buttonSave);

    _buttonCancel = new QPushButton("Cancel");
    _buttonCancel->setToolTip("Cancel global orientation changes");
    //connect(_buttonCancel, &QPushButton::clicked, this, &ModulesDialog::listItemCancelSave);
    _layoutButtonBox->addWidget(_buttonCancel);
    _layoutButtonBox->addStretch(1);

    connect(_buttonSave, SIGNAL(released()), this,
            SLOT(ok()));
    connect(_buttonCancel, SIGNAL(released()), this,
            SLOT(cancel()));

    _layoutWindow->addLayout(_layoutButtonBox);
}

void OrientationDialog::ok() {
    _orientationValue.x = QString(_linePitch->text()).toFloat();
    _orientationValue.y = QString(_lineYaw->text()).toFloat();
    _orientationValue.z = QString(_lineRoll->text()).toFloat();
    accept();
}

void OrientationDialog::cancel() {
    reject();
}

OrientationDialog::~OrientationDialog()
{
    delete _labelPitch;
    delete _labelRoll;
    delete _labelYaw;
    delete _validatorPitch;
    delete _validatorRoll;
    delete _validatorYaw;
    delete _linePitch;
    delete _lineRoll;
    delete _lineYaw;
    delete _buttonSave;
    delete _buttonCancel;
    delete _layoutPitch;
    delete _layoutRoll;
    delete _layoutYaw;
    delete _layoutButtonBox;
    delete _layoutWindow;
}

