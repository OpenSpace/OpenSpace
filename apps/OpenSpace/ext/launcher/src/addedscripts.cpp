#include "addedscripts.h"
#include "./ui_addedscripts.h"
#include <qevent.h>

addedScripts::addedScripts(std::string& imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::addedScripts)
    , _imported(imported)
{
    ui->setupUi(this);
    ui->text_scripts->setText(QString(_imported.c_str()));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseScript()));
}

void addedScripts::setScriptText(std::string s) {
    ui->text_scripts->setText(QString(s.c_str()));
}

void addedScripts::parseScript() {
    _imported = ui->text_scripts->toPlainText().toUtf8().constData();
    accept();
}

addedScripts::~addedScripts() { }
