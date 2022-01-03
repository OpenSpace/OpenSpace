#include "filesupport.h"

FileSupport::FileSupport(QVBoxLayout* parentLayout)
{
    _layoutButtonBox = new QHBoxLayout;
    _saveButton = new QPushButton("Save");
    _saveButton->setToolTip("Save global orientation changes");
    _layoutButtonBox->addStretch(1);
    _layoutButtonBox->addWidget(_saveButton);

    _cancelButton = new QPushButton("Cancel");
    _cancelButton->setToolTip("Cancel global orientation changes");
    //connect(_buttonCancel, &QPushButton::clicked, this, &ModulesDialog::listItemCancelSave);
    _layoutButtonBox->addWidget(_cancelButton);
    parentLayout->addLayout(_layoutButtonBox);
    connect(_cancelButton, SIGNAL(released()), this,
            SLOT(cancel()));
}

void FileSupport::cancel() {
    exit(0);
}

FileSupport::~FileSupport()
{
    delete _layoutButtonBox;
    delete _saveButton;
    delete _cancelButton;}

