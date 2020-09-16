#include "errordialog.h"
#include "ui_errordialog.h"
#include <QFileSystemModel>
#include <QScreen>
#include <sstream>


errordialog::errordialog(QString message, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::errordialog)
{
    ui->setupUi(this);
    ui->label->setText(message);
    //connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

errordialog::~errordialog() {
    delete ui;
}

//void errordialog::accept() {
//    reject();
//}
