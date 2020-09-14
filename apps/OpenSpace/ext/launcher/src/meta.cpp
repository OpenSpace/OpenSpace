#include <openspace/scene/profile.h>
#include "meta.h"
#include "./ui_meta.h"

meta::meta(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::meta)
    , _imported(imported)
{
    ui->setupUi(this);

    if (_imported->meta().has_value()) {
        ui->line_name->setText(QString(_imported->meta().value().name.c_str()));
        ui->line_version->setText(QString(_imported->meta().value().version.c_str()));
        ui->text_description->setText(QString(_imported->meta().value().description.c_str()));
        ui->line_author->setText(QString(_imported->meta().value().author.c_str()));
        ui->line_url->setText(QString(_imported->meta().value().url.c_str()));
        ui->line_license->setText(QString(_imported->meta().value().license.c_str()));
    }
    else {
        ui->line_name->setText("");
        ui->line_version->setText("");
        ui->text_description->setText("");
        ui->line_author->setText("");
        ui->line_url->setText("");
        ui->line_license->setText("");
    }

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(save()));
}

void meta::save() {
    if (!areAllEntriesBlank()) {
        openspace::Profile::Meta m;
        m.name = ui->line_name->text().toUtf8().constData();
        m.version = ui->line_version->text().toUtf8().constData();
        m.description = ui->text_description->toPlainText().toUtf8().constData();
        m.author = ui->line_author->text().toUtf8().constData();
        m.url = ui->line_url->text().toUtf8().constData();
        m.license = ui->line_license->text().toUtf8().constData();
	_imported->setMeta(m);
    }
    else {
        _imported->clearMeta();
    }
    accept();
}

bool meta::areAllEntriesBlank() {
    bool blank = true;
    if (ui->line_name->text().length() > 0) {
        blank = false;
    }
    if (ui->line_version->text().length() > 0) {
        blank = false;
    }
    if (ui->text_description->toPlainText().length() > 0) {
        blank = false;
    }
    if (ui->line_author->text().length() > 0) {
        blank = false;
    }
    if (ui->line_url->text().length() > 0) {
        blank = false;
    }
    if (ui->line_license->text().length() > 0) {
        blank = false;
    }
    return blank;
}

meta::~meta() {
    delete ui;
}
