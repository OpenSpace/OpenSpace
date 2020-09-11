#include "meta.h"
#include "./ui_meta.h"

meta::meta(Meta& imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::meta)
    , _imported(imported)
{
    ui->setupUi(this);

    ui->line_name->setText(QString(_imported.name.c_str()));
    ui->line_version->setText(QString(_imported.version.c_str()));
    ui->text_description->setText(QString(_imported.description.c_str()));
    ui->line_author->setText(QString(_imported.author.c_str()));
    ui->line_url->setText(QString(_imported.url.c_str()));
    ui->line_license->setText(QString(_imported.license.c_str()));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(save()));
}

void meta::save() {
    _imported.name = ui->line_name->text().toUtf8().constData();
    _imported.version = ui->line_version->text().toUtf8().constData();
    _imported.description = ui->text_description->toPlainText().toUtf8().constData();
    _imported.author = ui->line_author->text().toUtf8().constData();
    _imported.url = ui->line_url->text().toUtf8().constData();
    _imported.license = ui->line_license->text().toUtf8().constData();

    accept();
}

meta::~meta() {
    delete ui;
}
