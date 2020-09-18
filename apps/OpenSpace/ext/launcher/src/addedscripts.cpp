#include "addedscripts.h"
#include "./ui_addedscripts.h"
#include <qevent.h>
#include <iostream>
#include <sstream>
#include <QKeyEvent>

addedScripts::addedScripts(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::addedScripts)
    , _imported(imported)
{
    ui->setupUi(this);
    for (std::string s : _imported->additionalScripts()) {
        _data += s + "\n";
    }
    ui->text_scripts->setText(QString(_data.c_str()));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseScript()));
}

void addedScripts::setScriptText(std::string s) {
    ui->text_scripts->setText(QString(s.c_str()));
}

void addedScripts::parseScript() {
    std::vector<std::string> tmpMultilineStringToVector;
    std::istringstream iss(ui->text_scripts->toPlainText().toUtf8().constData());
    while (!iss.eof())
    {
        std::string s;
        getline(iss, s);
        tmpMultilineStringToVector.push_back(s);
    }
    _imported->setAdditionalScripts(tmpMultilineStringToVector);
    accept();
}

void addedScripts::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

addedScripts::~addedScripts() { }
